/* eslint global-require: 0, flowtype-errors/show-errors: 0 */

/**
 * This module executes inside of electron's main process. You can start
 * electron renderer process from here and communicate with the other processes
 * through IPC.
 *
 * When running `npm run build` or `npm run build-main`, this file is compiled to
 * `./app/main.prod.js` using webpack. This gives us some performance wins.
 *
 * @flow
 */
import { ipcMain, app, BrowserWindow, dialog, shell } from 'electron';
import * as path from 'path';

let mainWindow = null;

if (process.env.NODE_ENV === 'production') {
  const sourceMapSupport = require('source-map-support');
  sourceMapSupport.install();
}

if (
  process.env.NODE_ENV === 'development' ||
  process.env.DEBUG_PROD === 'true'
) {
  require('electron-debug')();
}

const installExtensions = async () => {
  const installer = require('electron-devtools-installer');
  const forceDownload = !!process.env.UPGRADE_EXTENSIONS;
  const extensions = ['REACT_DEVELOPER_TOOLS', 'REDUX_DEVTOOLS'];

  return Promise.all(
    extensions.map(name => installer.default(installer[name], forceDownload))
  ).catch(console.log);
};

/**
 * Add event listeners...
 */

app.on('window-all-closed', () => {
  // Respect the OSX convention of having the application in memory even
  // after all windows have been closed
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('ready', async () => {
  if (
    process.env.NODE_ENV === 'development' ||
    process.env.DEBUG_PROD === 'true'
  ) {
    await installExtensions();
  }

  mainWindow = new BrowserWindow({
    show: false,
    width: 1024,
    height: 728,
    minWidth: 640,
    minHeight: 480
  });

  mainWindow.loadURL(`file://${__dirname}/app.html`);

  // mainWindow.webContents.openDevTools();

  // @TODO: Use 'ready-to-show' event
  //        https://github.com/electron/electron/blob/master/docs/api/browser-window.md#using-ready-to-show-event
  mainWindow.webContents.on('did-finish-load', () => {
    if (!mainWindow) {
      throw new Error('"mainWindow" is not defined');
    }
    mainWindow.show();
    mainWindow.focus();
  });

  mainWindow.webContents.on('new-window', (event, url) => {
    event.preventDefault();
    shell.openExternal(url);
  });

  mainWindow.on('close', e => {
    e.preventDefault();
    mainWindow.webContents.send('control-channel', 'closing');
  });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });

  ipcMain.on('msg-channel', (event, arg) => {
    switch (arg) {
      case 'exit':
        mainWindow.destroy();
        break;
      case 'path':
        if (
          process.env.NODE_ENV === 'development' ||
          process.env.DEBUG_PROD === 'true'
        ) {
          event.sender.send(
            'path-channel',
            path.join(path.dirname(app.getPath('exe')), '..', '..', '..')
          );
        } else {
          event.sender.send('path-channel', path.dirname(app.getPath('exe')));
        }
        break;
      default:
        console.log(arg);
        break;
    }
  });

  ipcMain.on('error-channel', (event, arg) => {
    const title =
      (arg.isCaught ? 'Recoverable Error: ' : 'Fatal Error: ') + arg.errName;
    dialog.showMessageBox(
      {
        type: 'error',
        buttons: [],
        title: title,
        message: arg.errMsg
      },
      (resp, check) => {
        if (!arg.isCaught) {
          mainWindow.destroy();
        } else {
          dialog.showMessageBox({
            type: 'info',
            buttons: [],
            title: title,
            message: 'Restarting the game may solve this problem.'
          });
        }
      }
    );
  });
});
