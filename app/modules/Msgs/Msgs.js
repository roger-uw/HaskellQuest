import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import styles from './Msgs.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';
import Msg from './Msg';
import Options from './Options';

class Msgs extends Component {
  constructor(props) {
    super(props);
    this.bottomOfPage = React.createRef();
  }

  componentDidUpdate() {
    this.scrollToBottom();
  }

  scrollToBottom() {
    this.bottomOfPage.current.scrollIntoView({ behavior: 'smooth' });
  }

  render() {
    const { msgList, showOptions, optionDesc, handleClick } = this.props;
    return (
      <div className={styles.msgList}>
        {msgList.map((m, index) => (
          <Msg
            // can use index as key here because the order of elements never changes
            key={index}
            sender={m.sender}
            isAlt={m.isAlt}
            hasIcon={m.hasIcon}
            isInfo={m.isInfo}
            mainMsg={m.mainMsg}
            altMsg={m.altMsg}
            msgStyle={m.msgStyle}
            animDur={m.animDur}
          />
        ))}
        {showOptions ? (
          <div>
            <Options desc={optionDesc} onClick={handleClick} />
          </div>
        ) : null}
        <div className={styles.bottom} ref={this.bottomOfPage} />
      </div>
    );
  }
}

Msgs.propTypes = {
  msgList: PropTypes.arrayOf(
    PropTypes.shape({
      sender: PropTypes.string,
      isAlt: PropTypes.bool,
      hasIcon: PropTypes.bool,
      isInfo: PropTypes.bool,
      mainMsg: PropTypes.string,
      altMsg: PropTypes.string,
      msgStyle: PropTypes.string,
      animDur: PropTypes.number
    })
  ),
  showOptions: PropTypes.bool,
  optionDesc: PropTypes.arrayOf(PropTypes.string),
  handleClick: PropTypes.func
};

Msgs.defaultProps = {
  msgList: {
    sender: 'system',
    isAlt: false,
    hasIcon: false,
    isInfo: true,
    mainMsg: '',
    altMsg: null,
    msgStyle: 'systemMsg',
    animDur: 0.7
  },
  showOptions: false,
  optionDesc: null,
  handleClick: x => x
};

export default Msgs;
