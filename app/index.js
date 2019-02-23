import React from 'react';
import ReactDOM from 'react-dom';
import { ipcRenderer } from 'electron';
import './index.global.css';
import App from './modules/App/App';

window.onerror = function handleGlobalErrors(
  message,
  source,
  lineno,
  colno,
  error
) {
  ipcRenderer.send('error-channel', {
    errName: error.name,
    errMsg: error.message,
    isCaught: false
  });
  return true;
};

ReactDOM.render(<App />, document.getElementById('root'));
