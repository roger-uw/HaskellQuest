import React, { Component } from 'react';
import '../HaskellQuest.global.css';
import styles from './App.css';
import {
  startListener,
  reportCaughtError,
  reportFatalError,
  loadProcInfo,
  loadMsgList,
  saveProc,
  loadChpt,
  sendMsgToMain,
  execQuestion,
  saveQuestion,
  loadQuestion,
  loadQuestionList,
  genReport
} from '../GameIO/GameIO';
import Question from '../Question/Question';
import Selector from '../Question/Selector';
import Msgs from '../Msgs/Msgs';
import Popup from '../Popup/Popup';
import Dot from '../Popup/Dot';
import RewardBar from '../RewardBar/RewardBar';

class App extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentMsg: -1,
      currentSect: 0,
      currentChpt: 0,
      showOptions: false,
      showPopup: false,
      showQuestion: false,
      showSelector: false,
      newQuestionIndex: -1,
      totalReward: 0,
      timeTaken: 0,
      newChptIndex: -1
    };

    this.handleClick = this.handleClick.bind(this);
    this.saveGameProc = this.saveGameProc.bind(this);
    this.handleMainCtrl = this.handleMainCtrl.bind(this);
    this.handleSave = this.handleSave.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleBackToQuestionList = this.handleBackToQuestionList.bind(this);
    this.handleSelectQuestion = this.handleSelectQuestion.bind(this);
    this.handleExitQuestion = this.handleExitQuestion.bind(this);
    this.handleSelectChpt = this.handleSelectChpt.bind(this);

    this.timeoutID = null;
    this.questionTimer = null;
    this.msgAnimDur = 0.7;
    this.playerName = 'Harley';
    this.popupContent = '';
    this.chapter = {
      title: '',
      entryMark: 0.0,
      sections: [
        {
          tag: '...',
          messages: [],
          endDelay: 20.0,
          question: null,
          branch: {
            uncondTo: '...',
            options: []
          }
        }
      ]
    };
    this.question = {
      index: 0,
      tag: '',
      title: '',
      isOptional: true,
      reward: 0.0,
      expectedTime: 0,
      isTimed: false,
      template: '[--]',
      prefix: '',
      postfix: '',
      desc: '',
      saved: '[--]',
      editing: '[--]'
    };
    this.questionList = [];
    this.msgList = [];
    this.chptList = [];
    this.newChptInfo = null;
    this.questionSavingAction = Promise.resolve();
    this.gameProcSavingAction = Promise.resolve();
    this.evaluatingAction = Promise.resolve({ stat: 'AC', info: '' });
    this.setupAction = Promise.resolve();
  }

  componentDidMount() {
    startListener(this.handleMainCtrl);
    this.popupContent = 'loading';
    this.setState(
      {
        showPopup: true,
        showSelector: false
      },
      () => {
        this.setupAction = this.setupAction.then(() => this.setupChpt(-1));
        this.setupAction
          .then(() =>
            setTimeout(() => {
              this.popupContent = '';
              this.setState({
                showPopup: false,
                showSelector: true
              });
            }, 0)
          )
          .catch(err => {
            reportFatalError(err);
          });
      }
    );
  }

  async loadFiles(n) {
    const procInfo = await loadProcInfo();
    if (n >= 0) {
      procInfo.currentChpt = n;
    }
    const chpt = await loadChpt(procInfo.currentChpt);
    let msgList, questionList;
    if (procInfo.currentChpt >= procInfo.chptList.length) {
      msgList = await [];
      questionList = await [];
      procInfo.chptList.push({
        title: chpt.title,
        currentMsg: -1,
        currentSect: 0,
        entryMark: chpt.entryMark
      });
    } else {
      msgList = await loadMsgList(procInfo.currentChpt);
      questionList = await loadQuestionList(procInfo.currentChpt);
    }
    return {
      procInfo: procInfo,
      msgList: msgList,
      questionList: questionList,
      chpt: chpt
    };
  }

  async setupChpt(n) {
    const f = await this.loadFiles(n);
    this.chapter = f.chpt;
    this.questionList = f.questionList;
    this.chptList = f.procInfo.chptList;
    this.msgList = f.msgList;
    const { currentChpt } = f.procInfo;
    this.newChptInfo = await this.loadNewChptInfo();
    this.setState(
      {
        currentMsg: this.chptList[currentChpt].currentMsg,
        currentSect: this.chptList[currentChpt].currentSect,
        currentChpt: currentChpt,
        totalReward: f.procInfo.totalReward,
        newQuestionIndex: -1,
        newChptIndex: f.procInfo.newChptIndex
      },
      () => {
        // console.log('start fetching msg');
        this.fetchMsg(this.chptList[currentChpt].currentMsg + 1);
      }
    );
  }

  beforeExitChpt() {
    return Promise.all([
      this.evaluatingAction,
      this.questionSavingAction,
      this.gameProcSavingAction,
      this.setupAction
    ]).then(() => {
      this.gameProcSavingAction = this.gameProcSavingAction.then(() => {
        // console.log('clear timeout');
        clearTimeout(this.timeoutID);
        return this.saveGameProc();
      });
      return this.gameProcSavingAction;
    });
  }

  loadNewChptInfo() {
    return loadChpt(this.chptList.length)
      .then(chpt => ({ title: chpt.title, entryMark: chpt.entryMark }))
      .catch(err => null);
  }

  handleMainCtrl(arg) {
    switch (arg) {
      case 'closing':
        // console.log('enter mainCtrl');
        this.popupContent = 'saving';
        this.setState(
          {
            showQuestion: false,
            showPopup: true
          },
          () => {
            // console.log('enter saving');
            this.beforeExitChpt()
              .then(() => genReport())
              .then(() =>
                setTimeout(() => {
                  sendMsgToMain('exit');
                  this.popupContent = '';
                  this.setState({
                    showPopup: false
                  });
                }, 0)
              )
              .catch(err => {
                reportFatalError(err);
              });
          }
        );
        break;
      default:
        break;
    }
  }

  saveGameProc() {
    const {
      currentMsg,
      currentSect,
      currentChpt,
      totalReward,
      newChptIndex
    } = this.state;
    this.chptList[currentChpt].currentMsg = currentMsg;
    this.chptList[currentChpt].currentSect = currentSect;
    const proc = {
      procInfo: {
        chptList: this.chptList,
        currentChpt: currentChpt,
        totalReward: totalReward,
        newChptIndex: newChptIndex
      },
      msgList: this.msgList,
      questionList: this.questionList
    };
    return saveProc(proc);
  }

  formatMsg(m) {
    const isSystem = m.sender === 'system';
    const style = isSystem ? 'systemMsg' : 'npcMsg';
    const icon = !isSystem;
    const info = isSystem;
    return {
      sender: m.sender,
      isAlt: false,
      hasIcon: icon,
      isInfo: info,
      mainMsg: m.content,
      altMsg: null,
      msgStyle: style,
      animDur: this.msgAnimDur
    };
  }

  composeMsg(i) {
    const { currentSect } = this.state;
    const { options } = this.chapter.sections[currentSect].branch;
    return {
      sender: this.playerName,
      isAlt: false,
      hasIcon: true,
      isInfo: false,
      mainMsg: options[i].content,
      altMsg: options.length === 1 ? null : options[1 - i].content,
      msgStyle: 'playerMsg',
      animDur: this.msgAnimDur
    };
  }

  handleClick(i) {
    const { currentSect } = this.state;
    const nextTag = this.chapter.sections[currentSect].branch.options[i]
      .branchTo;
    const nextIndex = this.chapter.sections.findIndex(
      sec => sec.tag === nextTag
    );
    const newMsg = this.composeMsg(i);
    this.msgList.push(newMsg);
    this.setState(
      {
        showOptions: false,
        currentSect: nextIndex,
        currentMsg: -1
      },
      () => {
        this.gameProcSavingAction = this.gameProcSavingAction
          .then(() => this.saveGameProc())
          .catch(err => console.log(err.message));
        this.gameProcSavingAction
          .then(() => this.fetchMsg(-1))
          .catch(err => console.log(err.message));
      }
    );
  }

  presentQuestion(q) {
    let i = this.questionList.findIndex(x => x.tag === q.tag);
    if (i === -1) {
      this.questionList.push({
        tag: q.tag,
        isOptional: q.isOptional,
        title: q.title,
        isComplete: false,
        reward: q.reward,
        restReward: q.reward,
        isTimed: q.isTimed && !q.isOptional,
        expectedTime: q.expectedTime,
        timeTaken: 0,
        attempts: []
      });
      i = this.questionList.length - 1;
      this.setState({ newQuestionIndex: i });
    }
    // this.setState({newQuestionIndex: i});
    if (q.isOptional || this.questionList[i].isComplete) {
      const { currentSect } = this.state;
      this.handleBranch(this.chapter.sections[currentSect].branch);
    }
  }

  handleSave(d) {
    console.log('saved template', d);
    const { tag } = this.question;
    this.questionSavingAction = this.questionSavingAction
      .then(() => saveQuestion(tag, { template: d }))
      .catch(err => {
        reportFatalError(err);
      });
  }

  async handleSubmit(c) {
    console.log('submitted code', c);
    this.evaluatingAction = this.evaluatingAction.then(r =>
      execQuestion(
        this.question.tag,
        this.question.prefix + c + this.question.postfix
      )
    );
    const r = await this.evaluatingAction;
    const { showQuestion } = this.state;
    if (!showQuestion) {
      return { stat: 'INV', info: '' };
    }
    const i = this.question.index;
    if (!this.questionList[i].isComplete) {
      this.questionList[i].attempts.push(
        this.question.prefix + c + this.question.postfix
      );
      if (r.stat === 'AC') {
        clearInterval(this.questionTimer);
        if (this.question.isTimed) {
          const { timeTaken } = this.state;
          this.questionList[i].timeTaken = timeTaken;
        }
        this.questionList[i].isComplete = true;
        const finalReward = Math.max(
          this.question.reward,
          this.questionList[i].reward / 2
        );
        this.questionList[i].restReward = 0;
        this.question.reward = 0;
        this.questionList[i].isTimed = false;
        this.question.isTimed = false;
        const { totalReward } = this.state;
        this.setState({ totalReward: totalReward + finalReward });
        const { currentSect } = this.state;
        const b = this.chapter.sections[currentSect].branch;
        if (!this.question.isOptional) {
          this.handleBranch(b);
        } else {
          if (
            b.uncondTo == null &&
            (b.options == null || b.options.length === 0)
          ) {
            this.handleChptEnd();
          }
        }
      } else {
        const { totalReward } = this.state;
        this.questionList[i].restReward = Math.max(
          this.questionList[i].restReward * 0.8,
          this.questionList[i].reward / 2
        );
        this.question.reward = this.questionList[i].restReward;
        this.setState({ totalReward: totalReward + 0 });
      }
    }
    return r;
  }

  handleBackToQuestionList() {
    // this.question.tag = '';
    this.handleExitQuestion();
    this.setState({
      showQuestion: false
    });
  }

  handleExitQuestion() {
    clearInterval(this.questionTimer);
    const i = this.question.index;
    if (this.questionList[i].isTimed) {
      this.questionList[i].restReward = this.questionList[i].reward / 2;
      this.question.reward = this.questionList[i].restReward;
    }
    this.questionList[i].isTimed = false;
    this.question.isTimed = false;
  }

  handleSelectQuestion(i) {
    loadQuestion(this.questionList[i].tag)
      .then(loaded => {
        this.question.index = i;
        this.question.tag = this.questionList[i].tag;
        this.question.isOptional = this.questionList[i].isOptional;
        this.question.title = this.questionList[i].title;
        this.question.isTimed = this.questionList[i].isTimed;
        this.question.reward = this.questionList[i].restReward;
        this.question.expectedTime = this.questionList[i].expectedTime;
        this.question.template = loaded.template.template;
        this.question.prefix = loaded.template.prefix;
        this.question.postfix = loaded.template.postfix;
        this.question.desc = loaded.desc;
        this.question.saved = loaded.proc.template;
        this.question.editing = loaded.proc.template;
        const { newQuestionIndex } = this.state;
        const nqi = i === newQuestionIndex ? -1 : newQuestionIndex;
        // console.log(this.question);
        this.setState(
          {
            showQuestion: true,
            newQuestionIndex: nqi,
            timeTaken: 0
          },
          () => {
            if (this.question.isTimed) {
              this.questionTimer = setInterval(() => {
                const { timeTaken } = this.state;
                this.setState(
                  {
                    timeTaken: timeTaken + 1
                  },
                  () => {
                    const ix = this.question.index;
                    const {
                      reward,
                      expectedTime,
                      restReward
                    } = this.questionList[ix];
                    const { timeTaken: newTimeTaken } = this.state;
                    const degrade =
                      (0.5 *
                        reward *
                        Math.max(newTimeTaken - expectedTime, 0)) /
                      expectedTime;
                    this.question.reward = Math.max(
                      restReward - degrade,
                      reward / 2
                    );
                  }
                );
              }, 1000);
            }
          }
        );
        return i;
      })
      .catch(err => {
        reportFatalError(err);
      });
  }

  handleSecEnd() {
    const { currentSect } = this.state;
    if (this.chapter.sections[currentSect].question == null) {
      this.handleBranch(this.chapter.sections[currentSect].branch);
    } else {
      this.presentQuestion(this.chapter.sections[currentSect].question);
    }
  }

  handleBranch(b) {
    if (b.uncondTo != null) {
      const nextIndex = this.chapter.sections.findIndex(
        sec => sec.tag === b.uncondTo
      );
      this.setState({ currentSect: nextIndex, currentMsg: -1 }, () =>
        this.fetchMsg(-1)
      );
    } else {
      const { options } = b;
      const { showOptions } = this.state;
      if (options == null || options.length === 0) {
        this.optionDesc = [];
        if (showOptions) {
          this.setState({ showOptions: false });
        }
        this.handleChptEnd();
      } else {
        if (options.length === 1 || options[1] == null) {
          this.optionDesc = [options[0].content];
        } else {
          this.optionDesc = [options[0].content, options[1].content];
        }
        if (!showOptions) {
          this.setState({ showOptions: true });
        }
      }
    }
  }

  handleSelectChpt(n) {
    const { currentChpt, totalReward, newChptIndex } = this.state;
    if (n === currentChpt) {
      return;
    }
    let newTotalReward = totalReward;
    if (newChptIndex === n) {
      newTotalReward = totalReward - this.newChptInfo.entryMark;
    }
    this.popupContent = 'loading';
    this.setState(
      {
        showPopup: true,
        newChptIndex: newChptIndex === n ? -1 : newChptIndex,
        totalReward: newTotalReward
      },
      () => {
        this.beforeExitChpt()
          .then(() => {
            this.setupAction = this.setupAction.then(() => this.setupChpt(n));
            return this.setupAction;
          })
          .then(() =>
            setTimeout(() => {
              this.popupContent = '';
              this.setState({
                showPopup: false
              });
            }, 0)
          )
          .catch(err => {
            reportFatalError(err);
          });
      }
    );
  }

  handleChptEnd() {
    const { currentChpt, totalReward } = this.state;
    if (currentChpt < this.chptList.length - 1) {
      return;
    }
    if (this.newChptInfo != null) {
      if (totalReward >= this.newChptInfo.entryMark) {
        console.log('new chapter');
        this.setState({ newChptIndex: this.chptList.length });
      }
    } else {
      this.handleGameEnd();
    }
  }

  handleGameEnd() {
    // TODO: How will this game end?
    // return;
  }

  fetchMsg(i) {
    let index = i;
    const { currentSect } = this.state;
    const msgs = this.chapter.sections[currentSect].messages;
    if (index >= msgs.length) {
      this.timeoutID = setTimeout(() => this.handleSecEnd(), 0);
      return;
    }
    if (index >= 0) {
      const newMsg = this.formatMsg(msgs[index]);
      this.msgList.push(newMsg);
      this.setState({
        currentMsg: index
      });
    } else {
      index = -1;
    }
    if (index + 1 >= msgs.length) {
      const endDelay =
        Math.max(this.chapter.sections[currentSect].endDelay, this.msgAnimDur) *
        1000;
      this.timeoutID = setTimeout(() => this.handleSecEnd(), endDelay);
    } else {
      const msgDelay = Math.max(msgs[index + 1].delay, this.msgAnimDur) * 1000;
      this.timeoutID = setTimeout(() => this.fetchMsg(index + 1), msgDelay);
    }
  }

  render() {
    const {
      currentChpt,
      showOptions,
      showPopup,
      showQuestion,
      showSelector,
      newQuestionIndex,
      totalReward,
      timeTaken,
      newChptIndex
    } = this.state;
    return (
      <div className={styles.interface}>
        {showPopup ? (
          <Popup popupContent={this.popupContent} popupAnim={<Dot />} />
        ) : null}
        <div className={styles.rewardBarContainer}>
          <RewardBar reward={totalReward} />
        </div>
        <div className={styles.questionContainer}>
          {showQuestion ? (
            <Question
              saved={this.question.saved}
              template={this.question.template}
              head={this.question.title}
              desc={this.question.desc}
              handleSave={this.handleSave}
              handleSubmit={this.handleSubmit}
              handleBackToQuestionList={this.handleBackToQuestionList}
              handleExitQuestion={this.handleExitQuestion}
              newQuestionIndex={newQuestionIndex}
              isTimed={this.question.isTimed}
              timeTaken={timeTaken}
              reward={this.question.reward}
              expectedTime={this.question.expectedTime}
            />
          ) : showSelector ? (
            <Selector
              handleSelectQuestion={this.handleSelectQuestion}
              handleSelectChpt={this.handleSelectChpt}
              questionList={this.questionList}
              chptList={this.chptList}
              currentChpt={currentChpt}
              newQuestionIndex={newQuestionIndex}
              newChptIndex={newChptIndex}
              newChptInfo={this.newChptInfo}
            />
          ) : null}
        </div>
        <div className={styles.msgsContainer}>
          <Msgs
            msgList={this.msgList}
            showOptions={showOptions}
            optionDesc={this.optionDesc}
            handleClick={this.handleClick}
          />
        </div>
      </div>
    );
  }
}
// ========================================

export default App;
