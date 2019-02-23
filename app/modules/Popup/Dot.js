import React, { Component } from 'react';
import '../HaskellQuest.global.css';
import styles from './Dot.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

function Dot(props) {
  return <span className={styles.dot} />;
}

export default Dot;
