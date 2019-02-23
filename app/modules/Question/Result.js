import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './Result.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

class Result extends Component {
  render() {
    let text = '';
    let style = styles.result;
    const { status } = this.props;
    switch (status) {
      case 'EV':
        text = 'Sublimation in Progress . . . ';
        style = styles.resultEvaluating;
        break;
      case 'AC':
        text = 'Success!';
        style = styles.resultAccepted;
        break;
      case 'CE':
        text = 'Sublimator Error';
        style = styles.resultError;
        break;
      case 'WA':
        text = 'Wrong Operation Sequence';
        style = styles.resultError;
        break;
      case 'TLE':
        text = 'Operation Sequence Took Too Long';
        style = styles.resultTimeout;
        break;
      case 'RE':
        text = 'Invalid Operation Sequence';
        style = styles.resultError;
        break;
      default:
        text = 'Unknown Error';
        style = styles.resultError;
        break;
    }
    return (
      <div>
        <span className={style}> {text} </span>
      </div>
    );
  }
}

Result.propTypes = {
  status: PropTypes.oneOf(['EV', 'AC', 'CE', 'WA', 'TLE', 'RE'])
};

Result.defaultProps = {
  status: 'RE'
};

export default Result;
