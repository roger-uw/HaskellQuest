import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './Options.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';

function Options(props) {
  const { desc, onClick } = props;
  return (
    <div className={styles.options}>
      <button
        type="button"
        className={styles.option}
        onClick={() => onClick(0)}
      >
        <span> {desc[0]} </span>
      </button>
      {desc.length === 2 && desc[1] != null ? (
        <button
          type="button"
          className={styles.option}
          onClick={() => onClick(1)}
        >
          <span> {desc[1]} </span>
        </button>
      ) : null}
    </div>
  );
}

Options.propTypes = {
  desc: PropTypes.arrayOf(PropTypes.string),
  onClick: PropTypes.func
};

Options.defaultProps = {
  desc: [''],
  onClick: x => x
};

export default Options;
