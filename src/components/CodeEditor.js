import React, { Component } from 'react';
import {Controlled as CodeMirror} from 'react-codemirror2';
require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
//require('codemirror/theme/neat.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class CodeEditor extends Component {
    constructor(props){
        super(props);
        this.state = {
            source: this.props.method == null ? 'no source' : this.props.method.source,
        }
    }

    render() {
        return (
            <div>
            <p>{this.props.method.class}</p>
            <CodeMirror
                value={this.props.method.source}
                options={{
                    mode: 'smalltalk',
                    theme: 'material',
                    lineNumbers: true,
                    matchBrackets: true, 
                    indentUnit: 4, 
                    highlightSelectionMatches: true, 
                    styleActiveLine: true, 
                    matchTags: {
                        bothTags: true
                    }, 
                    lineWrapping: true, 
                    extraKeys: {
                        "Alt-S": "saveClicked", 
                        "Ctrl-S": "evaluateClicked"
                    }}}
                onBeforeChange={(editor, data, value) => { this.setState({source: value}) }}
                onChange={(editor, data, value) => {}}/>
            </div>
        )
    }
};

export default CodeEditor;
