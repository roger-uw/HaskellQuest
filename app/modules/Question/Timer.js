import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './Timer.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

class Timer extends Component {
  render() {
    const { time } = this.props;
    const style = time > 0 ? styles.timerCountdown : styles.timerCountup;
    return (
      <div className={styles.timerContainer}>
        <span className={style}> {`${Math.abs(time)}s`} </span>
      </div>
    );
  }
}

Timer.propTypes = {
  time: PropTypes.number
};

Timer.defaultProps = {
  time: 0
};

export default Timer;
