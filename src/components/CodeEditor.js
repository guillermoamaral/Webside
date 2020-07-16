import React, { Component } from 'react';
import { Grid, Paper, Box, IconButton } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import SaveIcon from '@material-ui/icons/CheckCircle';
import { Controlled as CodeMirror } from 'react-codemirror2';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class CodeEditor extends Component {
    constructor(props){
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            definition: props.definition,
            comment: props.comment,
            source: props.source,
            mode: "source",
            dirty: false,
            value: ''
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.definition !== state.definition ||
            props.comment !== state.comment ||
            props.source !== state.source) {
            return {
                definition: props.definition,
                comment: props.comment,
                source: props.source,
                value: props[state.mode],
            }
        };
        return null;
    }

    valueChanged = (value) => {
        this.setState({value: value, dirty: true})
    }

    modeChanged = (event, mode) => {
        this.setState({mode: mode, value: this.state[mode]})
    }

    define = (definition) => {
        const handler = this.props.onDefine;
        if (handler !== undefined) {
            handler(definition);
        }
    }
    
    comment = (comment) => {
        const handler = this.props.onComment;
        if (handler !== undefined) {
            handler(comment);
        }
    }

    compile = (source) => {
        const handler = this.props.onCompile;
        if (handler !== undefined) {
            handler(source);
        }
    }
    
    saveClicked = (event) => {
        const value = this.state.value;
        switch (this.state.mode) {
            case "comment":
                this.comment(value);
                break;
            case "definition":
                this.define(value);
                break;
            case "source":    
                this.compile(value);
                break;
            default:
        }
    }

    render() {
        return (
            <Grid container spacing={0}>
                <Grid item xs={12} md={12} lg={12}>
                    <ToggleButtonGroup
                        value={this.state.mode}
                        exclusive
                        onChange={this.modeChanged}>
                        <ToggleButton value="source" variant="outlined" size="small">
                            Method defintion
                        </ToggleButton>
                        <ToggleButton value="definition" variant="outlined" size="small">
                            Class definition
                        </ToggleButton>
                        <ToggleButton value="comment" variant="outlined" size="small">
                            Class comment
                        </ToggleButton>
                    </ToggleButtonGroup>    
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={0}>
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
                                            "Alt-S": "saveClicked", 
                                            "Ctrl-S": "evaluateClicked"
                                        }}}
                                    onBeforeChange={(editor, data, value) => {this.valueChanged(value)}}
                                    onChange={(editor, data, value) => {this.valueChanged(value)}}
                                />
                            </Paper>
                        </Grid>
                        <Grid item xs={1} md={1} lg={1}>
                            <Box display="flex" justifyContent="center"> 
                                <IconButton color="inherit" onClick={this.saveClicked}>
                                    <SaveIcon size="large" style={{fontSize: 30}}/>
                                </IconButton>
                            </Box>
                        </Grid>
                    </Grid>
                </Grid>
            </Grid>
        )
    }
};

export default CodeEditor;

