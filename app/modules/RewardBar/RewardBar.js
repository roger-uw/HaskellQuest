import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './RewardBar.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

function RewardBar(props) {
  const { reward } = props;
  return <span className={styles.rewardBar}> {reward.toFixed(2)} </span>;
}

RewardBar.propTypes = {
  reward: PropTypes.number
};

RewardBar.defaultProps = {
  reward: 0
};

export default RewardBar;
