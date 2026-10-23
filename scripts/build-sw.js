// scripts/build-sw.js -- генерирует public/sw.<hash>.js после shadow-cljs release.
//
// Не парсит manifest.edn (формат EDN, отдельный парсер ради одного имени
// файла того не стоит) -- вместо этого сканирует public/js/ на файл
// main.<HASH>.js и public/fonts/ на всё содержимое, и вписывает актуальные
// пути прямо в сгенерированный sw.js. Так список ресурсов гарантированно
// совпадает с тем, что shadow-cljs реально положил на диск в этом релизе,
// без риска устаревшего руками вписанного хэша.
//
// Имя самого файла sw.<hash>.js версионируется хэшем -- это надёжнее,
// чем полагаться на побайтовое сравнение неизменного /sw.js браузером:
// смена URL скрипта сама по себе триггерит install нового SW, минуя
// все эвристики HTTP-кэша (критично для iOS Safari). Клиент узнаёт
// актуальное имя через sw-version.json, который должен отдаваться
// с Cache-Control: no-store.

const fs = require("fs");
const path = require("path");
const crypto = require("crypto");

const PUBLIC_DIR = path.join(__dirname, "..", "public");
const JS_DIR = path.join(PUBLIC_DIR, "js");
const FONTS_DIR = path.join(PUBLIC_DIR, "fonts");
const MANIFEST_PATH = path.join(PUBLIC_DIR, "sw-version.json");

function findMainBundle() {
  const bundlePath = path.join(JS_DIR, "main.js");
  if (!fs.existsSync(bundlePath)) {
    throw new Error(`build-sw: не нашёл main.js в ${JS_DIR}`);
  }
  return "/js/main.js";
}

function listFonts() {
  if (!fs.existsSync(FONTS_DIR)) return [];
  return fs.readdirSync(FONTS_DIR).map((f) => `/fonts/${f}`);
}

// favicon кэшируем "навсегда" (пока не изменится сам файл) -- он попадает
// и в SHELL_ASSETS (кэшируется при install, отдаётся из кэша мгновенно),
// и в cacheVersion (значит смена байтов иконки сама даст новый хэш sw.js
// и, как следствие, новый CACHE_NAME -- старая иконка уйдёт вместе со
// старым кэшем на activate, без отдельной ревалидации в рантайме)
function findFavicon() {
  const iconPath = path.join(PUBLIC_DIR, "favicon.ico");
  return fs.existsSync(iconPath) ? "/favicon.ico" : null;
}

// версия кэша -- хэш от содержимого ВСЕХ ресурсов шелла, а не только бандла:
// если поменялся index.html или шрифт при том же JS (например, правка через
// build-html.js без пересборки cljs), версия всё равно должна обновиться,
// иначе SW продолжит отдавать старый файл из уже закэшированной версии
function cacheVersion(assets) {
  const hash = crypto.createHash("sha256");
  for (const asset of assets.sort()) {
    if (asset === "/" || asset === "/app") continue; // "/" -- то же самое, что index.html
    const full = path.join(PUBLIC_DIR, asset);
    hash.update(fs.readFileSync(full));
  }
  return hash.digest("hex").slice(0, 12);
}

function renderSw(assets, version) {
  const assetsJson = JSON.stringify(assets, null, 2);
  return `// sw.js -- СГЕНЕРИРОВАН scripts/build-sw.js, не редактировать руками.
// Правь scripts/build-sw.js и пересобирай (npm run release).

const CACHE_NAME = "nam-shell-${version}";

const SHELL_ASSETS = ${assetsJson};

// Гонка health-check пингов ВСЕГДА между одной и той же фиксированной парой
// -- del.24nam.ru и namdel.ru -- независимо от того, под каким origin сейчас
// выполняется сам SW. del.24nam.ru тут чисто сигнальный маршрут ("жив ли
// бэкенд хоть где-то"): он никогда не bывает self.location.origin (SW под
// ним не регистрируют) и статику с него никогда не тянем напрямую -- это
// тот же бэкенд через туннельный CF-маршрут, гонять по нему трафик статики
// не хотим. Победитель гонки транслируется в реальный источник статики:
//   del.24nam.ru выиграл -> берём статику с 24nam.ru (независимое CF Pages
//                            зеркало)
//   namdel.ru выиграл     -> берём статику с namdel.ru (он же и есть)
const PING_A = "https://del.24nam.ru";
const PING_B = "https://namdel.ru";

const STATIC_FOR_PING_WINNER = {
  [PING_A]: "https://24nam.ru",
  [PING_B]: "https://namdel.ru",
};

// origin'ы, под которыми вообще может выполняться сам SW в проде -- на них
// и распространяется гонка/фолбэк выше. Остальное (например, localhost при
// локальной разработке) обслуживается напрямую, без гонки.
const KNOWN_ORIGINS = new Set(["https://namdel.ru", "https://24nam.ru"]);

// PWA на namdel.ru живёт под /app (и /app/... для внутренних роутов), на
// 24nam.ru -- под корнем. Обычные файловые ассеты (/js/..., /fonts/...)
// одинаковы на обоих доменах и под этот паттерн не подпадают, так что
// translatePath их не трогает -- перевод нужен только для входной точки.
const toStaticMirrorPath = (pathname) => // namdel.ru "/app..." -> 24nam.ru "/..."
  pathname === "/app" || pathname.startsWith("/app/") ? pathname.slice(4) || "/" : pathname;

const toNamdelPath = (pathname) => // 24nam.ru "/" -> namdel.ru "/app"
  pathname === "/" || pathname === "/index.html" ? "/app" : pathname;

// путь переводится из раскладки "own" в раскладку "target" -- own важен:
// "/" и "/app" -- это ОДИН И ТОТ ЖЕ логический вход (app-шелл), просто под
// разными путями на разных origin'ах, поэтому без own нельзя понять, что
// именно переводить (см. installShell ниже, где это раньше приводило к
// порче кэша при кросс-origin фолбэке во время install).
function translatePath(own, target, pathname) {
  if (own === target) return pathname;
  if (target === "https://24nam.ru") return toStaticMirrorPath(pathname);
  if (target === "https://namdel.ru") return toNamdelPath(pathname);
  return pathname;
}

// зеркально ping-timeout-ms/request-timeout-ms в core.cljs: health-check
// пинг лёгкий и должен отваливаться быстро, реальная загрузка ассета --
// не обязана быть такой же быстрой, ей нужен запас побольше
const PING_TIMEOUT_MS = 5000;
const REQUEST_TIMEOUT_MS = 15000;

// withTimeout НЕ просто гонится через Promise.race с таймером -- в этом
// случае fetch, проигравший гонку, продолжает висеть в фоне никуда не
// отменённый. На сети, где домен не отвечает явным отказом (TCP RST), а
// просто "чернит" пакеты (тот же паттерн DPI-блокировки, что и на клиенте),
// таких зависших fetch'ей за одну попытку обновления накапливается сразу
// несколько параллельно -- и на мобильном браузере с более тесным лимитом
// одновременных соединений, чем у десктопного Chrome, это забивает пул
// соединений и выглядит как полное зависание, хотя каждый отдельный вызов
// вроде бы должен был отвалиться по таймауту. Поэтому вместо голого fetch
// принимаем фабрику (signal) => fetch(..., {signal}) и реально отменяем
// её через AbortController по истечении ms, а не просто игнорируем итог.
function withTimeout(fetchFn, ms) {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), ms);
  return fetchFn(controller.signal).finally(() => clearTimeout(timer));
}

// гонка по health-check пингам вместо перебора по очереди: берём маршрут,
// чей пинг первым ответил ok, а не ждём таймаут на каждом недоступном по
// очереди, прежде чем дойти до живого.
// Promise.race тут не годится напрямую -- он резолвится и на reject тоже,
// а нам нужен именно первый УСПЕШНЫЙ; поэтому считаем settled вручную
// и падаем только когда отвалились вообще все маршруты. Проигравшие
// маршруты явно абортим (controller.abort()), как только победитель
// найден -- незачем держать их сокеты открытыми до истечения таймаута.
function raceFastestRoute(routes, ms) {
  return new Promise((resolve, reject) => {
    if (routes.length === 0) {
      reject(new Error("raceFastestRoute: пустой список маршрутов"));
      return;
    }
    let remaining = routes.length;
    let settled = false;
    const controllers = routes.map(() => new AbortController());
    const abortOthers = (winnerIdx) =>
      controllers.forEach((c, i) => i !== winnerIdx && c.abort());
    const onFail = () => {
      remaining -= 1;
      if (remaining === 0 && !settled) {
        reject(new Error("raceFastestRoute: все маршруты недоступны"));
      }
    };
    routes.forEach((route, i) => {
      const timer = setTimeout(() => controllers[i].abort(), ms);
      fetch(route + "/", { cache: "no-store", signal: controllers[i].signal }).then(
        (res) => {
          clearTimeout(timer);
          if (res.ok && !settled) {
            settled = true;
            abortOthers(i);
            resolve(route);
          } else {
            onFail();
          }
        },
        () => {
          clearTimeout(timer);
          onFail();
        }
      );
    });
  });
}

// пингуем фиксированную пару PING_A/PING_B и возвращаем реальный источник
// статики (STATIC_FOR_PING_WINNER победителя), либо null, если не ответил
// ни один из двух -- тогда работаем офлайн на уже закэшированной версии.
function resolveStaticTarget() {
  return raceFastestRoute([PING_A, PING_B], PING_TIMEOUT_MS)
    .then((winner) => STATIC_FOR_PING_WINNER[winner])
    .catch(() => null);
}

async function installShell() {
  const own = self.location.origin;
  const target = KNOWN_ORIGINS.has(own) ? await resolveStaticTarget() : own;
  if (!target) return; // ни один маршрут не ответил -- офлайн на старом кэше

  // "/" и "/app" в SHELL_ASSETS -- один и тот же логический вход
  // (app-шелл), просто под разными путями на разных origin'ах.
  // Кэшировать нужно ТОЛЬКО тот, что реально соответствует own --
  // иначе если во время install гонка пингов случайно уводит на чужой
  // target, под собственным ключом own ("/", если own=24nam.ru) может
  // закэшироваться ответ с ЧУЖОГО пути на том target (например,
  // health-check-текст с namdel.ru), который потом будет отдаваться
  // как ответ на реальную навигацию на этот own -- именно так и
  // возникал баг с "healthy" вместо приложения на 24nam.ru.
  const ownAppPath = own === "https://24nam.ru" ? "/" : "/app";
  const assetsToCache = SHELL_ASSETS.filter((a) => a !== "/" && a !== "/app").concat(ownAppPath);

  const cache = await caches.open(CACHE_NAME);
  // allSettled, а не addAll: один зависший/недоступный ресурс не должен
  // рушить установку целиком, и на каждый есть свой таймаут (с реальной
  // отменой через AbortController, см. withTimeout)
  await Promise.allSettled(
    assetsToCache.map((asset) =>
      withTimeout(
        (signal) =>
          fetch(target + translatePath(own, target, asset), { cache: "no-store", signal }),
        REQUEST_TIMEOUT_MS
      ).then((res) => {
        if (res.ok) return cache.put(asset, res);
      })
    )
  );
}

self.addEventListener("install", (event) => {
  event.waitUntil(installShell());
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches.keys().then((keys) =>
      Promise.all(
        keys
          .filter((key) => key !== CACHE_NAME)
          .map((key) => caches.delete(key))
      )
    )
  );
  self.clients.claim();
});

async function handleFetch(event, own, url) {
  const cached = await caches.match(event.request,
    { ignoreSearch: event.request.mode === "navigate" });
  if (cached) return cached;

  const offline = () =>
    new Response("Сервер недоступен, приложение работает офлайн.", {
      status: 503,
      statusText: "Offline",
      headers: { "Content-Type": "text/plain; charset=utf-8" },
    });

  if (!KNOWN_ORIGINS.has(own)) {
    // origin без гонки (в проде не ожидается, например localhost) --
    // просто обычный fetch с таймаутом
    return withTimeout((signal) => fetch(event.request, { signal }), REQUEST_TIMEOUT_MS)
      .catch(offline);
  }

  const fetchFromTarget = (target) =>
    withTimeout(
      (signal) =>
        fetch(target + translatePath(own, target, url.pathname) + url.search, {
          cache: "no-store",
          signal,
        }),
      REQUEST_TIMEOUT_MS
    ).catch(offline);

  const ownController = new AbortController();
  const ownTimer = setTimeout(() => ownController.abort(), REQUEST_TIMEOUT_MS);
  const ownAttempt = fetch(event.request, { signal: ownController.signal }).finally(() =>
    clearTimeout(ownTimer)
  );

  // гонка ДВУХ ОДИНАКОВО лёгких пингов (PING_A vs PING_B) против реального
  // fetch'а ассета с "own" -- не пинг против пинга, это было бы нечестной
  // гонкой, пинг выигрывал бы всегда просто по размеру запроса. Если пинг-
  // гонка решила раньше, чем ownAttempt успел сам отработать, и её
  // результат указывает на другой источник статики -- ownAttempt
  // прерывается немедленно, вместо того чтобы ждать свои полные
  // REQUEST_TIMEOUT_MS впустую, когда результат уже известен.
  const pingRace = resolveStaticTarget();

  let outcome;
  try {
    outcome = await Promise.race([
      ownAttempt.then((res) => ({ kind: "own", res })),
      pingRace.then((target) => ({ kind: "ping", target })),
    ]);
  } catch {
    // ownAttempt сам отклонился раньше, чем пинг-гонка успела ответить --
    // дожидаемся её результата и идём за ассетом на найденный источник
    const target = await pingRace;
    return target ? fetchFromTarget(target) : offline();
  }

  if (outcome.kind === "own") return outcome.res;
  if (outcome.target === own) return ownAttempt; // свой маршрут жив -- просто ждём его дальше
  ownController.abort(); // свой маршрут не выиграл -- ждать нечего
  ownAttempt.catch(() => {});
  return outcome.target ? fetchFromTarget(outcome.target) : offline();
}

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  const own = url.origin;
  const isApiCall =
    url.pathname === "/send" ||
    url.pathname === "/inbox" ||
    url.pathname === "/ack" ||
    url.pathname === "/call-ws" ||
    url.pathname.startsWith("/push/");
  const isHealthCheck = own === "https://namdel.ru" && url.pathname === "/";
  if (isApiCall || isHealthCheck) return;
  event.respondWith(handleFetch(event, own, url));
});

self.addEventListener("push", (event) => {
  console.log("PUSH EVENT FIRED", event.data && event.data.text());
  const nickname = event.data ? event.data.text() : "";
  event.waitUntil(
    self.registration.showNotification("Входящий звонок", {
      body: nickname || "Откройте приложение, чтобы ответить",
      tag: "call",
      renotify: true,
      requireInteraction: true,
      icon: "/icons/icon-192.png",
      badge: "/icons/badge-72.png",
      data: { nickname },
    }).then(() => console.log("showNotification OK"))
      .catch(err => console.error("showNotification FAILED", err))
  );
});

self.addEventListener("notificationclick", (event) => {
  const { nickname } = event.notification.data || {};
  event.notification.close();
  const base = self.location.origin === "https://24nam.ru" ? "/" : "/app";
  const start = nickname ? base + "?call=" + encodeURIComponent(nickname) : base;
  event.waitUntil(
    self.clients.matchAll({ type: "window", includeUncontrolled: true }).then((wins) => {
      for (const w of wins) {
        if ("navigate" in w) return w.navigate(start).then((c) => c.focus());
        if ("focus" in w) return w.focus();
      }
      return self.clients.openWindow(start);
    })
  );
});
`;
}

function cleanupOldServiceWorkers(currentFile) {
  fs.readdirSync(PUBLIC_DIR)
    .filter((f) => /^sw\.[0-9a-f]{12}\.js$/.test(f))
    .filter((f) => f !== currentFile)
    .forEach((f) => {
      fs.unlinkSync(path.join(PUBLIC_DIR, f));
      console.log(`build-sw: removed stale ${f}`);
    });
}

function main() {
  const bundlePath = findMainBundle();
  const favicon = findFavicon();
  const assets = [
    "/",
    "/app",
    "/index.html",
    bundlePath,
    ...listFonts(),
    ...(favicon ? [favicon] : []),
  ];
  const version = cacheVersion(assets);
  const swFile = `sw.${version}.js`;
  const swPath = path.join(PUBLIC_DIR, swFile);

  fs.writeFileSync(swPath, renderSw(assets, version));
  fs.writeFileSync(MANIFEST_PATH, JSON.stringify({ file: `/${swFile}` }));
  cleanupOldServiceWorkers(swFile);

  console.log(
    `build-sw: wrote ${swPath} (cache nam-shell-${version}, ${assets.length} assets)`,
  );
}

main();
