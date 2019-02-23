import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './Selector.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

class Selector extends Component {
  constructor(props) {
    super(props);

    const { chptList, currentChpt } = this.props;
    this.state = {
      head: chptList[currentChpt].title
    };

    this.handleMouseOver = this.handleMouseOver.bind(this);
    this.handleMouseLeave = this.handleMouseLeave.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.holdTimeout = null;
  }

  componentWillUnmount() {
    clearTimeout(this.holdTimeout);
  }

  handleClick(i) {
    if (i < 0) {
      return;
    }
    const { handleSelectChpt } = this.props;
    handleSelectChpt(i);
  }

  handleMouseOver(i) {
    // console.log('mouseover');
    const { newChptInfo, newChptIndex, chptList } = this.props;
    clearTimeout(this.holdTimeout);
    let newHead = '';
    if (i < 0) {
      newHead = `completion with at least ${
        newChptInfo.entryMark
      } ponectors is required`;
    } else {
      if (i === newChptIndex) {
        newHead = newChptInfo.title;
      } else {
        newHead = chptList[i].title;
      }
    }
    this.holdTimeout = setTimeout(() => {
      this.setState({
        head: newHead
      });
    }, 200);
  }

  handleMouseLeave(i) {
    const { chptList, currentChpt } = this.props;
    clearTimeout(this.holdTimeout);
    this.holdTimeout = setTimeout(() => {
      this.setState({
        head: chptList[currentChpt].title
      });
    }, 300);
  }

  render() {
    const {
      handleSelectQuestion,
      questionList,
      chptList,
      currentChpt,
      newQuestionIndex,
      newChptIndex,
      newChptInfo
    } = this.props;
    const { head } = this.state;
    const chptCount = chptList.length + (newChptInfo == null ? 0 : 1);
    const dynamicWidth = {
      width: `calc(100% / ${chptCount} - 3em)`
    };
    return (
      <div className={styles.selectorContainer}>
        <div className={styles.selectorHead}>
          <h1> {'HaskellQuest'} </h1>
          <h5> {head} </h5>
          <div className={styles.chptSelector}>
            {chptList.map((c, index) => (
              <button
                type="button"
                className={
                  currentChpt === index ? styles.currentChpt : styles.otherChpt
                }
                style={dynamicWidth}
                // can use index as key here because the order of elements never changes
                key={index}
                onMouseOver={() => this.handleMouseOver(index)}
                onMouseLeave={() => this.handleMouseLeave(index)}
                onFocus={() => this.handleMouseOver(index)}
                onBlur={() => this.handleMouseLeave(index)}
                onClick={() => this.handleClick(index)}
              />
            ))}
            {newChptInfo != null ? (
              <button
                type="button"
                className={
                  newChptIndex >= 0
                    ? styles.newChptAvailable
                    : styles.newChptUnavailable
                }
                style={dynamicWidth}
                onMouseOver={() => this.handleMouseOver(newChptIndex)}
                onMouseLeave={() => this.handleMouseLeave(newChptIndex)}
                onFocus={() => this.handleMouseOver(newChptIndex)}
                onBlur={() => this.handleMouseLeave(newChptIndex)}
                onClick={() => this.handleClick(newChptIndex)}
              />
            ) : null}
          </div>
        </div>
        <div className={styles.selector}>
          {questionList.map((q, index) => (
            <button
              type="button"
              className={
                newQuestionIndex === index ? styles.selectNew : styles.select
              }
              // can use index as key here because the order of elements never changes
              key={index}
              onClick={() => handleSelectQuestion(index)}
            >
              {q.isComplete ? null : <span className={styles.incompleteDot} />}
              {q.isTimed ? <span className={styles.timedDot} /> : null}
              <span> {q.title} </span>
            </button>
          ))}
        </div>
      </div>
    );
  }
}

Selector.propTypes = {
  handleSelectQuestion: PropTypes.func,
  handleSelectChpt: PropTypes.func,
  questionList: PropTypes.arrayOf(
    PropTypes.shape({
      tag: PropTypes.string,
      isOptional: PropTypes.bool,
      title: PropTypes.string,
      isComplete: PropTypes.bool,
      reward: PropTypes.number,
      restReward: PropTypes.number,
      isTimed: PropTypes.bool,
      expectedTime: PropTypes.number,
      timeTaken: PropTypes.number,
      attempts: PropTypes.arrayOf(PropTypes.string)
    })
  ),
  chptList: PropTypes.arrayOf(
    PropTypes.shape({
      title: PropTypes.string,
      entryMark: PropTypes.number,
      sections: PropTypes.arrayOf(
        PropTypes.shape({
          tag: PropTypes.string,
          messages: PropTypes.arrayOf(
            PropTypes.shape({
              sender: PropTypes.string,
              delay: PropTypes.number,
              content: PropTypes.string
            })
          ),
          endDelay: PropTypes.number,
          question: PropTypes.shape({
            tag: PropTypes.string,
            title: PropTypes.string,
            reward: PropTypes.number,
            expectedTime: PropTypes.number,
            isTimed: PropTypes.bool,
            isOptional: PropTypes.bool
          }),
          branch: PropTypes.shape({
            uncondTo: PropTypes.string,
            options: PropTypes.arrayOf(
              PropTypes.shape({
                content: PropTypes.string,
                branchTo: PropTypes.string
              })
            )
          })
        })
      )
    })
  ),
  currentChpt: PropTypes.number,
  newQuestionIndex: PropTypes.number,
  newChptIndex: PropTypes.number,
  newChptInfo: PropTypes.shape({
    title: PropTypes.string,
    entryMark: PropTypes.number
  })
};

Selector.defaultProps = {
  handleSelectQuestion: x => x,
  handleSelectChpt: x => x,
  questionList: [],
  chptList: [],
  currentChpt: 0,
  newQuestionIndex: -1,
  newChptIndex: -1,
  newChptInfo: {
    title: '',
    entryMark: 0
  }
};

export default Selector;
