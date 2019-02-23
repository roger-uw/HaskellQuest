import * as path from 'path';
import * as fs from 'fs';
import { ipcRenderer } from 'electron';
import { execFile } from 'child_process';
import writeFileAtomic from 'write-file-atomic';

export function startListener(f) {
  ipcRenderer.on('control-channel', (event, arg) =>
    setTimeout(() => f(arg), 0)
  );
}

function waitPath() {
  return new Promise((resolve, reject) => {
    ipcRenderer.once('path-channel', (event, arg) => resolve(arg));
    setTimeout(() => reject(new Error('Timeout')), 1000);
  });
}

export function pathGetter() {
  const p = waitPath();
  ipcRenderer.send('msg-channel', 'path');
  return p;
}

function loadJSON(p) {
  return new Promise((resolve, reject) => {
    fs.readFile(p, 'utf8', (err, data) => {
      if (err) {
        reject(err);
      } else {
        let jsonData;
        try {
          jsonData = JSON.parse(data);
        } catch (e) {
          reject(e);
        }
        resolve(jsonData);
      }
    });
  });
}

export async function genReport() {
  const [proc, m0, m1, q0, q1] = await Promise.all([
    loadProcInfo(),
    loadMsgList(0),
    loadMsgList(1),
    loadQuestionList(0),
    loadQuestionList(1)
  ]);
  const report = {
    proc: proc,
    q0: q0,
    q1: q1,
    m0: m0,
    m1: m1
  };
  return writeJSON('../HaskellQuest_Report.json', report);
}

export function loadProcInfo() {
  return loadJSON('./app/assets/proc/proc.json');
}

export function loadMsgList(c) {
  return loadJSON(`./app/assets/proc/msg${c}.json`);
}

export function loadQuestionList(c) {
  return loadJSON(`./app/assets/proc/question${c}.json`);
}

export function loadChpt(c) {
  return loadJSON(`./app/assets/story/chpt${c}.json`);
}

export function reportCaughtError(e) {
  ipcRenderer.send('error-channel', {
    errName: e.name,
    errMsg: e.message,
    isCaught: true
  });
}

export function reportFatalError(e) {
  ipcRenderer.send('error-channel', {
    errName: e.name,
    errMsg: e.message,
    isCaught: false
  });
}

function writeJSON(p, d) {
  return new Promise((resolve, reject) => {
    writeFileAtomic(p, JSON.stringify(d), err => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

export function saveProc(p) {
  return Promise.all([
    writeJSON('./app/assets/proc/proc.json', p.procInfo),
    writeJSON(`./app/assets/proc/msg${p.procInfo.currentChpt}.json`, p.msgList),
    writeJSON(
      `./app/assets/proc/question${p.procInfo.currentChpt}.json`,
      p.questionList
    )
  ]);
}

export function sendMsgToMain(m) {
  ipcRenderer.send('msg-channel', m);
}

function fixImagePath(n, s) {
  if (
    process.env.NODE_ENV === 'development' ||
    process.env.DEBUG_PROD === 'true'
  ) {
    return s.replace(/__QPATH__/g, `./assets/questions/${n}`);
  } else {
    return s.replace(/__QPATH__/g, `../../app/assets/questions/${n}`);
  }
}

export async function loadQuestion(n) {
  const [qTemplate, qDesc, qProc] = await Promise.all([
    loadJSON(`./app/assets/questions/${n}/template.json`),
    loadText(`./app/assets/questions/${n}/desc.md`),
    loadJSON(`./app/assets/questions/${n}/proc.json`).catch(err =>
      Promise.resolve({ template: null })
    )
  ]);
  if (qProc.template == null) {
    qProc.template = qTemplate.template;
  }
  return {
    template: qTemplate,
    desc: fixImagePath(n, qDesc),
    proc: qProc
  };
}

export function saveQuestion(n, q) {
  return writeJSON(`./app/assets/questions/${n}/proc.json`, q);
}

function loadText(p) {
  return new Promise((resolve, reject) => {
    fs.readFile(p, 'utf8', (err, data) => {
      if (err) {
        reject(err);
      } else {
        resolve(data);
      }
    });
  });
}

function writeText(p, d) {
  return new Promise((resolve, reject) => {
    writeFileAtomic(p, d, err => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

function analyseGHCError(e) {
  return e;
}

function runCode(exePath, n) {
  return new Promise((resolve, reject) => {
    execFile(
      'stack',
      [
        'ghc',
        '--',
        '-e',
        'Main.main',
        `${exePath}/app/assets/questions/${n}/Composed.hs`,
        `${exePath}/app/assets/questions/GenJSONInfo.hs`
      ],
      (error, stdout, stderr) => {
        if (error) {
          // console.log(error);
          resolve({ stat: 'CE', info: analyseGHCError(error.message) });
        } else {
          let result;
          try {
            result = JSON.parse(stdout);
          } catch (e) {
            reject(e);
          }
          resolve(result);
        }
      }
    );
  });
}

async function composeCode(n, c) {
  const qMain = await loadText(`./app/assets/questions/${n}/Main.hs`);
  return writeText(
    `./app/assets/questions/${n}/Composed.hs`,
    `${qMain}\n\n${c}`
  );
}

export async function execQuestion(n, c) {
  const exePath = await pathGetter();
  console.log('Env: ', exePath);
  await composeCode(n, c);
  const r = await runCode(exePath, n);
  return r;
}
