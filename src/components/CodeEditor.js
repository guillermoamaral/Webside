import React, { Component } from 'react';
import { Grid, Paper, Box, IconButton } from '@material-ui/core';
import AcceptIcon from '@material-ui/icons/CheckCircle';
import { Controlled as CodeMirror } from 'react-codemirror2';
import PopupMenu from './PopupMenu';
import { AppContext } from '../AppContext';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class CodeEditor extends Component {
    static contextType = AppContext;

    constructor(props){
        super(props);
        this.instance = null;
        this.state = {
            source: props.source,
            dirty: false,
            value: props.source
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.source !== state.source) {
            return {
                source: props.source,
                value: props.source,
            }
        };
        return null;
    }

    valueChanged = (value) => {
        const handler = this.props.onChange;
        if (handler !== undefined) {handler(value)}
        this.setState({value: value, dirty: true})
    }
    
    acceptClicked = (event) => {
        const handler = this.props.onAccept;
        if (handler !== undefined) {handler(this.state.value)}
    }

    browseSenders = () => {
        const selector = this.instance.getSelection();
        this.context.browseSenders(selector);
    }

    browseImplementors = () => {
        const selector = this.instance.getSelection();
        this.context.browseImplementors(selector);
    }

    browseReferences = () => {
        const global = this.instance.getSelection();
        this.context.browseReferences(global);
    }

    evaluableExpression() {
        const expression = this.instance.getSelection();
        if (expression.length > 0) {return expression}
        const cursor = this.instance.getCursor("to");
        return this.instance.getRange({ch: 0, line: cursor.line}, cursor)
    }

    evaluate = () => {
        const expression = this.evaluableExpression();
        this.context.api.evaluate(expression, false)
    }

    inspect = () => {
        const expression = this.evaluableExpression();
        this.context.api.evaluate(expression, true)
            .then(object => {
                this.context.inspectObject(object);            })
    }

    show = () => {
        const expression = this.evaluableExpression();
        this.context.api.evaluate(expression, false)
            .then(object => {
                const cursor = this.instance.getCursor("to");
                this.instance.replaceRange(" " + object.printString, cursor);
                const from = {ch: cursor.ch + 1, line: cursor.line};
                const to = {ch: from.ch + object.printString.length, line: from.line};
                this.instance.setSelection(from, to)
            })
    }
  
    render() {
        return (
            <Grid container spacing={1}>
                <Grid item xs={11} md={11} lg={11}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.classes.codeMirror}
                            value={this.state.value}
                            options={{
                                mode: 'smalltalk',
                                theme: 'material',
                                lineSeparator: '\r',
                                lineNumbers: true,
                                matchBrackets: true, 
                                indentUnit: 10, 
                                highlightSelectionMatches: true, 
                                styleActiveLine: true, 
                                matchTags: {
                                    bothTags: true
                                }, 
                                lineWrapping: true, 
                                extraKeys: {
                                    "Ctrl-D": this.evaluate,
                                    "Ctrl-I": this.inspect,
                                    "Ctrl-S": this.show,
                                    "Alt-S": this.acceptClicked,
                                    "Alt-N": this.browseSenders,
                                    "Alt-M": this.browseImplementors,
                                    "Alt-R": this.browseReferences
                                }}}
                            editorDidMount={editor => {this.instance = editor}}
                            onBeforeChange={(editor, data, value) => {this.valueChanged(value)}}
                            onChange={(editor, data, value) => {this.valueChanged(value)}}
                        />
                    </Paper>
                </Grid>
                <Grid item xs={1} md={1} lg={1}>
                    <Box display="flex" justifyContent="center" > 
                        <IconButton color="inherit" onClick={this.acceptClicked}>
                            <AcceptIcon size="large" style={{fontSize: 30}}/>
                        </IconButton>
                    </Box>
                </Grid>
            </Grid>
        )
    }
};

export default CodeEditor;

