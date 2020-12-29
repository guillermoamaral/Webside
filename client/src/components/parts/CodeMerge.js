import React, { Component } from 'react';
import CodeMirror from 'codemirror';
require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/theme/smalltalk.css');
require('codemirror/mode/smalltalk/smalltalk.js');
require('codemirror/addon/search/searchcursor.js');
require('codemirror/addon/search/search.js');
require('codemirror/addon/search/jump-to-line.js');
require('codemirror/addon/search/match-highlighter.js');
require('codemirror/addon/edit/matchbrackets.js');
require('codemirror/addon/edit/closebrackets.js');
require('codemirror/addon/comment/comment.js');
require('codemirror/addon/selection/active-line.js');
require('codemirror/addon/display/fullscreen.js');
require('codemirror/addon/display/fullscreen.css');
require('codemirror/addon/scroll/annotatescrollbar.js');
require('codemirror/addon/lint/lint.js');
require('codemirror/addon/lint/lint.css');
require('codemirror/addon/merge/merge.js');
require('codemirror/addon/merge/merge.css');
require('codemirror/addon/fold/foldgutter.css');
require('codemirror/addon/fold/foldcode');
require('codemirror/addon/fold/brace-fold');
require('codemirror/addon/fold/comment-fold');

class CodeMerge extends Component {
    componentDidMount() {
        var target = this.refs['react-diff-code-view'];
        console.log(target)
        target.innerHTML = "";
        CodeMirror.MergeView(target, Object.assign({}, {
          readOnly: true,
          lineNumbers: true,
          theme: 'material',
          value: this.props.leftCode,
          orig: this.props.rightCode,
          mode: "smalltalk",
          highlightDifferences: "highlight",
          connect: null,
          revertButtons: false,
          styleActiveLine: true,
          lineWrap: true,
          smartIndent: true,
          matchBrackets: true,
          foldGutter:true,
          gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"]}, this.props.options || {}));
    }
    
    render() {
        return (
            <div ref="react-diff-code-view">
                
            </div>
        )
    }
}

export default CodeMerge;