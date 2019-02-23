import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ReactMarkdown from 'react-markdown';
import '../HaskellQuest.global.css';
import styles from './Question.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';
import Editor from './Editor';
import Result from './Result';
import Timer from './Timer';

class Question extends Component {
  constructor(props) {
    super(props);
    this.state = { refresh: false, status: '' };
    this.handleClear = this.handleClear.bind(this);
    this.handleReload = this.handleReload.bind(this);
    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleBackToQuestionList = this.handleBackToQuestionList.bind(this);
    this.reload = true;
    this.code = '';
  }

  componentWillUnmount() {
    const { handleExitQuestion } = this.props;
    handleExitQuestion();
    // console.log('Question will unmount');
  }

  handleClear() {
    const { status } = this.state;
    if (status === 'EV') {
      return;
    }
    this.setState(
      {
        refresh: true,
        status: ''
      },
      () =>
        setTimeout(() => {
          this.reload = false;
          this.setState({
            refresh: false,
            status: ''
          });
        }, 0)
    );
  }

  handleReload() {
    const { status } = this.state;
    if (status === 'EV') {
      return;
    }
    this.setState(
      {
        refresh: true,
        status: ''
      },
      () =>
        setTimeout(() => {
          this.reload = true;
          this.setState({
            refresh: false,
            status: ''
          });
        }, 0)
    );
  }

  handleBackToQuestionList() {
    const { status } = this.state;
    const { handleBackToQuestionList } = this.props;
    if (status === 'EV') {
      return;
    }
    handleBackToQuestionList();
  }

  handleChange(c) {
    this.code = c;
    const { status } = this.state;
    if (status !== '') {
      this.setState({ status: '' });
    }
  }

  handleSubmit() {
    const { status } = this.state;
    const { handleSubmit } = this.props;
    if (status === 'EV') {
      return;
    }
    this.setState(
      {
        status: 'EV'
      },
      () => {
        handleSubmit(this.code)
          .then(r => {
            if (r.stat !== 'INV') {
              this.setState({ status: r.stat });
            }
            return r;
          })
          .catch(err => {
            reportFatalError(err);
          });
      }
    );
  }

  render() {
    const {
      saved,
      template,
      head,
      desc,
      handleSave,
      newQuestionIndex,
      isTimed,
      timeTaken,
      reward,
      expectedTime
    } = this.props;
    const { refresh, status } = this.state;
    return (
      <div className={styles.question}>
        {isTimed ? <Timer time={Math.round(expectedTime - timeTaken)} /> : null}
        <button
          type="button"
          className={newQuestionIndex >= 0 ? styles.backNew : styles.back}
          onClick={this.handleBackToQuestionList}
        >
          <span> {'Back'} </span>
        </button>
        <ReactMarkdown
          className={styles.questionHead}
          source={`# ${head}\`${reward.toFixed(2)}\``}
          linkTarget="_blank"
        />
        <ReactMarkdown
          className={styles.questionDesc}
          source={desc}
          linkTarget="_blank"
        />
        <div className={styles.editorAndButtons}>
          <button
            type="button"
            className={styles.reload}
            onClick={this.handleReload}
          >
            <span> {'Reload'} </span>
          </button>
          <button
            type="button"
            className={styles.clear}
            onClick={this.handleClear}
          >
            <span> {'Clear'} </span>
          </button>
          <div className={styles.editor}>
            {refresh ? null : (
              <Editor
                template={this.reload ? saved : template}
                handleChange={this.handleChange}
                handleSave={handleSave}
                isLocked={status === 'EV'}
              />
            )}
          </div>
          <button
            type="button"
            className={styles.submit}
            onClick={this.handleSubmit}
          >
            <span> {'Sublimate'} </span>
          </button>
          {status === '' ? null : <Result status={status} />}
        </div>
      </div>
    );
  }
}

Question.propTypes = {
  saved: PropTypes.string,
  template: PropTypes.string,
  head: PropTypes.string,
  desc: PropTypes.string,
  handleSave: PropTypes.func,
  handleSubmit: PropTypes.func,
  handleBackToQuestionList: PropTypes.func,
  handleExitQuestion: PropTypes.func,
  newQuestionIndex: PropTypes.number,
  isTimed: PropTypes.bool,
  timeTaken: PropTypes.number,
  reward: PropTypes.number,
  expectedTime: PropTypes.number
};

Question.defaultProps = {
  saved: '[--]',
  template: '[--]',
  head: '',
  desc: '',
  handleSave: () => {},
  handleSubmit: x => x,
  handleBackToQuestionList: () => {},
  handleExitQuestion: () => {},
  newQuestionIndex: -1,
  isTimed: false,
  timeTaken: 0,
  reward: 0,
  expectedTime: 0
};

export default Question;
