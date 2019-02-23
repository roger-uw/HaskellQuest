import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './Popup.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';
import Dot from './Dot';

function Popup(props) {
  const { popupContent, popupAnim } = props;
  return (
    <div className={styles.popup}>
      <div className={styles.popupAnim}> {popupAnim} </div>
      <div className={styles.popupContent}> {popupContent} </div>
    </div>
  );
}

Popup.propTypes = {
  popupContent: PropTypes.string,
  popupAnim: PropTypes.object
  /*
  (props, propName) => {
    if (props[propName] && !isValidElementType(props[propName])) {
      return new Error(
        `Invalid prop 'popupAnim' supplied to 'Popup': the prop is not a valid React component`
      );
    }
  }
  */
};

Popup.defaultProps = {
  popupContent: '',
  popupAnim: <Dot />
};

export default Popup;
