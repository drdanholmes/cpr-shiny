// cp-R desktop shell: starts the bundled R running the Shiny app on a free
// local port, shows a splash screen until it answers, then loads it.

const { app, BrowserWindow, dialog, shell } = require('electron');
const { spawn, execFile } = require('child_process');
const path = require('path');
const net = require('net');
const http = require('http');
const fs = require('fs');

const STARTUP_TIMEOUT_MS = 90 * 1000;

// Packaged: R and the app live in resources/. Dev (`npm start`): R is in
// electron/r-win and the app is the repo root.
const RESOURCES = app.isPackaged ? process.resourcesPath : __dirname;
const R_HOME = path.join(RESOURCES, app.isPackaged ? 'R' : 'r-win');
const APP_DIR = app.isPackaged
  ? path.join(process.resourcesPath, 'app')
  : path.join(__dirname, '..');
const RSCRIPT = path.join(R_HOME, 'bin', 'Rscript.exe');

let rProcess = null;
let rLog = '';
let mainWindow = null;
let splash = null;
let quitting = false;

function freePort() {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.unref();
    srv.on('error', reject);
    srv.listen(0, '127.0.0.1', () => {
      const { port } = srv.address();
      srv.close(() => resolve(port));
    });
  });
}

function startR(port) {
  // Use only the bundled library and ignore any user .Rprofile/.Renviron, so
  // packages from a separately installed R can't interfere. Kept on one line
  // with single quotes: Rscript.exe -e mangles newlines and double quotes.
  const expr = [
    '.libPaths(.Library)',
    "shiny::runApp(Sys.getenv('CPR_APP_DIR'), " +
      "port = as.integer(Sys.getenv('CPR_PORT')), " +
      "host = '127.0.0.1', launch.browser = FALSE)"
  ].join('; ');

  rProcess = spawn(RSCRIPT, ['--vanilla', '-e', expr], {
    env: {
      ...process.env,
      R_HOME,
      CPR_APP_DIR: APP_DIR,
      CPR_PORT: String(port),
      R_LIBS: '',
      R_LIBS_USER: '',
      R_LIBS_SITE: ''
    },
    windowsHide: true
  });

  const keep = (chunk) => { rLog = (rLog + chunk.toString()).slice(-8000); };
  rProcess.stdout.on('data', keep);
  rProcess.stderr.on('data', keep);

  rProcess.on('exit', (code) => {
    rProcess = null;
    if (!quitting) {
      dialog.showErrorBox('cp-R stopped',
        `The R process exited unexpectedly (code ${code}).\n\n${rLog.slice(-3000)}`);
      app.quit();
    }
  });
}

function stopR() {
  if (!rProcess) return;
  // Rscript.exe launches R as a child process, so kill the whole tree.
  execFile('taskkill', ['/pid', String(rProcess.pid), '/T', '/F'], () => {});
  rProcess = null;
}

function waitForShiny(url) {
  const deadline = Date.now() + STARTUP_TIMEOUT_MS;
  return new Promise((resolve, reject) => {
    const attempt = () => {
      if (quitting) return reject(new Error('quitting'));
      http.get(url, (res) => {
        res.resume();
        if (res.statusCode === 200) resolve();
        else retry();
      }).on('error', retry);
    };
    const retry = () => {
      if (Date.now() > deadline) {
        reject(new Error(`Shiny did not start within ${STARTUP_TIMEOUT_MS / 1000} s.`));
      } else {
        setTimeout(attempt, 300);
      }
    };
    attempt();
  });
}

function createSplash() {
  splash = new BrowserWindow({
    width: 420, height: 240, frame: false, resizable: false,
    alwaysOnTop: true, show: true
  });
  splash.loadFile(path.join(__dirname, 'splash.html'));
}

function createMainWindow(url) {
  mainWindow = new BrowserWindow({
    width: 1400, height: 900, show: false,
    title: 'cp-R',
    autoHideMenuBar: true,
    webPreferences: { contextIsolation: true, nodeIntegration: false }
  });

  // Shiny download buttons use target="_blank": download those here; send
  // any other link to the user's default browser.
  mainWindow.webContents.setWindowOpenHandler(({ url: target }) => {
    if (target.startsWith(url)) mainWindow.webContents.downloadURL(target);
    else shell.openExternal(target);
    return { action: 'deny' };
  });

  mainWindow.once('ready-to-show', () => {
    if (splash) { splash.destroy(); splash = null; }
    mainWindow.show();
  });
  mainWindow.on('closed', () => { mainWindow = null; });
  mainWindow.loadURL(url);
}

async function boot() {
  if (!fs.existsSync(RSCRIPT)) {
    dialog.showErrorBox('cp-R', `Bundled R not found at:\n${RSCRIPT}`);
    app.quit();
    return;
  }

  createSplash();
  const port = await freePort();
  const url = `http://127.0.0.1:${port}/`;
  startR(port);

  try {
    await waitForShiny(url);
  } catch (err) {
    if (quitting) return;
    dialog.showErrorBox('cp-R failed to start', `${err.message}\n\n${rLog.slice(-3000)}`);
    app.quit();
    return;
  }
  createMainWindow(url);
}

if (!app.requestSingleInstanceLock()) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
  app.whenReady().then(boot);
  app.on('before-quit', () => { quitting = true; stopR(); });
  app.on('window-all-closed', () => app.quit());
}
