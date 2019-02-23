import React, { Component } from 'react';
import PropTypes from 'prop-types';
import '../HaskellQuest.global.css';
import './Msg.global.css';
import styles from './Msg.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';
import MsgBox from './MsgBox';

class Msg extends Component {
  constructor(props) {
    super(props);
    this.isNew = true;
  }

  componentDidUpdate() {
    this.isNew = false;
  }

  choiceToStatus(c) {
    const { isInfo } = this.props;
    if (isInfo) {
      return 'info';
    } else {
      return c ? 'active' : 'inactive';
    }
  }

  msgStyleConv() {
    const { msgStyle } = this.props;
    switch (msgStyle) {
      case 'playerMsg':
        return styles.playerMsg;
      case 'npcMsg':
        return styles.npcMsg;
      case 'systemMsg':
        return styles.systemMsg;
      default:
        return styles.systemMsg;
    }
  }

  render() {
    const {
      sender,
      isAlt,
      hasIcon,
      isInfo,
      mainMsg,
      altMsg,
      animDur
    } = this.props;
    const icon = hasIcon ? (
      <img src={require(`../../assets/images/${sender}.png`)} alt="" />
    ) : null;
    const animStyle = this.isNew
      ? {
          animationName: 'msg-trans',
          animationDuration: `${animDur}s`,
          animationFillMode: 'both',
          animationTimingFunction: 'ease-out'
        }
      : { position: 'relative' };
    return (
      <div className={this.msgStyleConv()} style={animStyle}>
        {hasIcon ? <div className={styles.iconContainer}> {icon} </div> : null}
        <div className={styles.msgBody}>
          {isInfo ? null : <div className={styles.msgTag}> {sender} </div>}
          <div className={styles.msgContent}>
            <MsgBox status={this.choiceToStatus(!isAlt)} content={mainMsg} />
            {altMsg ? (
              <MsgBox status={this.choiceToStatus(isAlt)} content={altMsg} />
            ) : null}
          </div>
        </div>
      </div>
    );
  }
}

Msg.propTypes = {
  sender: PropTypes.string,
  isAlt: PropTypes.bool,
  hasIcon: PropTypes.bool,
  isInfo: PropTypes.bool,
  mainMsg: PropTypes.string,
  altMsg: PropTypes.string,
  msgStyle: PropTypes.string,
  animDur: PropTypes.number
};

Msg.defaultProps = {
  sender: 'system',
  isAlt: false,
  hasIcon: false,
  isInfo: true,
  mainMsg: '',
  altMsg: null,
  msgStyle: 'systemMsg',
  animDur: 0.7
};

export default Msg;
