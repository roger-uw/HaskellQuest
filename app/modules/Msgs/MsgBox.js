import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './MsgBox.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

function genMsgBoxStyle(s) {
  switch (s) {
    case 'active':
      return styles.activeMsg;
    case 'inactive':
      return styles.inactiveMsg;
    case 'info':
      return styles.infoMsg;
    default:
      return styles.activeMsg;
  }
}

function MsgBox(props) {
  const { status, content } = props;
  const style = genMsgBoxStyle(status);
  return (
    <div className={style}>
      <span> {content} </span>
    </div>
  );
}

MsgBox.propTypes = {
  status: PropTypes.oneOf(['active', 'inactive', 'info']),
  content: PropTypes.string
};

MsgBox.defaultProps = {
  status: 'info',
  content: ''
};

export default MsgBox;
