import React, { Component } from 'react';
import PropTypes from 'prop-types';
// import brace from 'brace';
import AceEditor from 'react-ace';
import '../HaskellQuest.global.css';
import styles from './Editor.css';
import { reportCaughtError, reportFatalError } from '../GameIO/GameIO';
import 'brace/mode/haskell';
import 'brace/theme/github';
import 'brace/theme/terminal';
import 'brace/theme/monokai';

function templateToDoc(original) {
  const leftBra = '[-';
  const rightBra = '-]';
  const source = original.split('\n');
  const result = [];
  const anchors = [];
  let startRow = 0,
    startCol = 0,
    endRow = 0,
    endCol = 0,
    k = 0;
  let startShift = 0,
    endShift = 0;
  while (startRow < source.length) {
    startCol = source[startRow].indexOf(leftBra, k);
    if (startCol === -1) {
      const lineEnd = startRow === source.length - 1 ? '' : '\n';
      result.push(source[startRow].slice(k), lineEnd);
      startRow += 1;
      k = 0;
      startShift = 0;
      endShift = 0;
    } else {
      result.push(source[startRow].slice(k, startCol));
      endRow = startRow;
      k = startCol + leftBra.length;
      endShift += leftBra.length;
      while (endRow < source.length) {
        endCol = source[endRow].indexOf(rightBra, k);
        if (endCol === -1) {
          const lineEnd = endRow === source.length - 1 ? '' : '\n';
          result.push(source[endRow].slice(k), lineEnd);
          endRow += 1;
          k = 0;
          endShift = 0;
        } else {
          result.push(source[endRow].slice(k, endCol));
          const anchorStartCol = startCol - startShift - 1;
          const anchorEndCol = endCol - endShift + 1;
          anchors.push({
            start: { row: startRow, column: anchorStartCol },
            end: { row: endRow, column: anchorEndCol }
          });
          break;
        }
      }
      startRow = endRow;
      k = endCol + rightBra.length;
      startShift = endShift + rightBra.length;
      endShift = startShift;
    }
  }
  return {
    anchors: anchors,
    doc: result.join('')
  };
}

function docToTemplate(doc, ranges) {
  const leftBra = '[-';
  const rightBra = '-]';
  let rangeN = 0;
  const lines = doc.split('\n');
  const result = [];
  let endRow = 0,
    endCol = 0,
    startRow = 0,
    startCol = 0;
  while (rangeN < ranges.length) {
    startRow = ranges[rangeN].start.row;
    startCol = ranges[rangeN].start.column + 1;
    while (endRow < startRow) {
      result.push(lines[endRow].slice(endCol), '\n');
      endRow += 1;
      endCol = 0;
    }
    result.push(lines[startRow].slice(endCol, startCol), leftBra);
    endRow = ranges[rangeN].end.row;
    endCol = ranges[rangeN].end.column - 1;
    while (startRow < endRow) {
      result.push(lines[startRow].slice(startCol), '\n');
      startRow += 1;
      startCol = 0;
    }
    result.push(lines[endRow].slice(startCol, endCol), rightBra);
    rangeN += 1;
  }
  while (endRow < lines.length) {
    const lineEnd = endRow === lines.length - 1 ? '' : '\n';
    result.push(lines[endRow].slice(endCol), lineEnd);
    endRow += 1;
    endCol = 0;
  }
  return result.join('');
}

class Editor extends Component {
  constructor(props) {
    super(props);
    this.handleReadOnly = this.handleReadOnly.bind(this);
    this.saveTemplate = this.saveTemplate.bind(this);
    this.ranges = [];
    this.freeMod = false;
    this.aceEditor = React.createRef();
  }

  componentDidMount() {
    const { template, handleChange } = this.props;
    this.aceEditor.current.editor.setOption(
      'fontFamily',
      '"Courier New", Courier, monospace'
    );
    this.aceEditor.current.editor.setOption('showLineNumbers', false);
    this.aceEditor.current.editor.setOption('maxLines', 80);
    this.aceEditor.current.editor.renderer.setScrollMargin(15, 15, 15, 15);
    this.docSetup(template);
    this.aceEditor.current.editor.session.doc.on('change', this.handleReadOnly);
    handleChange(this.aceEditor.current.editor.session.doc.getValue());
    this.intervalID = setInterval(this.saveTemplate, 60000);
    // console.log(this.aceEditor.current.editor);
    this.aceEditor.current.editor.commands.addCommand({
      name: 'InvalidUndo',
      bindKey: { win: 'Ctrl-Z', mac: 'Command-Z' },
      exec: function doNothing(editor) {},
      readOnly: false
    });
    this.aceEditor.current.editor.commands.addCommand({
      name: 'InvalidRedo',
      bindKey: { win: 'Ctrl-Shift-Z|Ctrl-Y', mac: 'Command-Shift-Z|Command-Y' },
      exec: function doNothing(editor) {},
      readOnly: false
    });
  }

  componentWillUnmount() {
    clearInterval(this.intervalID);
    this.saveTemplate();
    // console.log('Editor will unmount');
  }

  getRawAnchorPos(a) {
    return {
      row: a.row,
      column: a.column
    };
  }

  isInRange(p, r) {
    return !(
      r.start.row > p.start.row ||
      r.end.row < p.start.row ||
      (r.start.row === p.start.row && r.start.column >= p.start.column) ||
      (r.end.row === p.start.row && r.end.column <= p.start.column)
    );
  }

  handleReadOnly(e) {
    if (this.freeMod) {
      return;
    }
    if (this.undo) {
      // console.log('recover');
      let k = 0;
      for (; k < this.ranges.length; k++) {
        this.ranges[k].startAnchor.setPosition(
          this.ranges[k].start.row,
          this.ranges[k].start.column,
          true
        );
        this.ranges[k].endAnchor.setPosition(
          this.ranges[k].end.row,
          this.ranges[k].end.column,
          true
        );
      }
      this.undo = false;
    } else {
      // console.log('modify', e);
      // console.log(this.ranges);
      const ix = this.ranges.findIndex(r => this.isInRange(e, r));
      const fail =
        ix === -1 ||
        (e.action === 'remove' &&
          (this.ranges[ix].end.row < e.end.row ||
            (this.ranges[ix].end.row === e.end.row &&
              this.ranges[ix].end.column <= e.end.column)));
      if (fail) {
        this.undo = true;
        this.aceEditor.current.editor.session.doc.revertDeltas([e]);
      } else {
        const { handleChange } = this.props;
        let k = 0;
        for (; k < this.ranges.length; k++) {
          this.ranges[k].start = this.getRawAnchorPos(
            this.ranges[k].startAnchor
          );
          this.ranges[k].end = this.getRawAnchorPos(this.ranges[k].endAnchor);
        }
        handleChange(this.aceEditor.current.editor.session.doc.getValue());
      }
    }
  }

  docSetup(template) {
    const t = templateToDoc(template);
    this.ranges.forEach(r => {
      r.startAnchor.detach();
      r.endAnchor.detach();
    });
    this.ranges = [];
    this.freeMod = true;
    this.aceEditor.current.editor.session.doc.setValue(t.doc);
    this.aceEditor.current.editor.setReadOnly(true);
    setTimeout(() => {
      this.freeMod = false;
    }, 0);
    t.anchors.forEach(a => {
      const startAnchor = this.aceEditor.current.editor.session.doc.createAnchor(
        a.start.row,
        a.start.column
      );
      const endAnchor = this.aceEditor.current.editor.session.doc.createAnchor(
        a.end.row,
        a.end.column
      );
      startAnchor.setPosition(a.start.row, a.start.column, true);
      endAnchor.setPosition(a.end.row, a.end.column, true);
      this.ranges.push({
        startAnchor: startAnchor,
        endAnchor: endAnchor,
        start: this.getRawAnchorPos(startAnchor),
        end: this.getRawAnchorPos(endAnchor)
      });
    });
    this.undo = false;
    this.aceEditor.current.editor.setReadOnly(false);
  }

  saveTemplate() {
    const { handleSave } = this.props;
    this.aceEditor.current.editor.setReadOnly(true);
    const code = this.aceEditor.current.editor.session.doc.getValue();
    const r = [];
    const n = this.ranges.length;
    let i = 0;
    while (i < n) {
      r.push({
        start: {
          row: this.ranges[i].start.row,
          column: this.ranges[i].start.column
        },
        end: { row: this.ranges[i].end.row, column: this.ranges[i].end.column }
      });
      i += 1;
    }
    this.aceEditor.current.editor.setReadOnly(false);
    handleSave(docToTemplate(code, r));
  }

  render() {
    const { isLocked } = this.props;
    return (
      <AceEditor
        ref={this.aceEditor}
        mode="haskell"
        theme="monokai"
        name="HaskellQuestEditor"
        width="50vw"
        fontSize="1.5vw"
        // maxLines = {80}
        tabSize={2}
        showGutter={false}
        showPrintMargin={false}
        readOnly={isLocked}
        editorProps={{ $blockScrolling: Infinity }}
      />
    );
  }
}

Editor.propTypes = {
  template: PropTypes.string,
  handleChange: PropTypes.func,
  handleSave: PropTypes.func,
  isLocked: PropTypes.bool
};

Editor.defaultProps = {
  template: '[--]',
  handleChange: x => x,
  handleSave: x => x,
  isLocked: true
};

export default Editor;
