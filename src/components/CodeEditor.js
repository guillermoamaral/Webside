import React, { Component } from 'react';
import { Grid, Paper, Toolbar, IconButton } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import SaveIcon from '@material-ui/icons/Save';
import { Controlled as CodeMirror } from 'react-codemirror2';
import axios from 'axios';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class CodeEditor extends Component {
    constructor(props){
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            class: '"no class"',
            definition: '"no class definition"', 
            comment: '"no class comment"',
            category: null,
            selector: null,
            source: '"no source code"',
            mode: "source",
            dirty: false,
            value: ''
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.class !== state.class ||
            props.definition !== state.definition ||
            props.comment !== state.comment ||
            props.category !== state.category ||
            props.selector !== state.selector ||
            props.source !== state.source) {
            return {
                class: props.class,
                definition: props.definition,
                comment: props.comment,
                category: props.category,
                selector: props.selector,
                source: props.source,
                value: props[state.mode],
            }
        };
        return null;
    }

    changed = (value) => {
        this.setState({value: value, dirty: true})
    }

    changeMode = (e, mode) => {
        this.setState({mode: mode, value: this.state[mode]})
    }

    defineClass = (definition) => {
        axios.post(this.props.baseUri + '/classes', definition)
            .then(res => {
                const accepted = res.data;
                if (this.props !== null && this.props.onClassDefined !== undefined) { 
                    const handler = this.props.onClassDefined;
                    handler.bind(this);
                    handler(accepted)
                }
                this.setState({definition: accepted})})
            .catch(error => {this.reportError(error)})
    }
    
    commentClass = (comment) => {
        axios.post(this.props.baseUri + '/classes/' + this.props.class + '/comment', comment)
            .then(res => {
                const accepted = res.data;
                this.setState({comment: accepted})
                if (this.props !== null && this.props.onClassCommented !== undefined) { 
                    const handler = this.props.onClassCommented;
                    handler.bind(this);
                    handler(accepted)
                }})
            .catch(error => {this.reportError(error)})
    }

    compileMethod = (source) => {
        const method = {source: source, category: this.props.category};
        axios.post(this.props.baseUri + '/classes/' + this.props.class + '/methods', method)
            .then(res => {
                const compiled = res.data;
                this.setState({source: compiled.source, selector: compiled.selector})
                if (this.props !== null && this.props.onMethodCompiled !== undefined) { 
                    const handler = this.props.onMethodCompiled;
                    handler.bind(this);
                    handler(compiled)
                }})
            .catch(error => {this.reportError(error)})
    }
    
    saveClicked = (e) => {
        const { value } = this.state;
        switch (this.state.mode) { 
            case "comment":
                this.commentClass(value);
                break;
            case "definition":
                this.defineClass(value);
                break;
            case "source":    
                this.compileMethod(value);
                break;
            default:
        }
    }

    render() {
        return (
            <Grid container spacing={0}>
                <Grid item xs={12} md={12} lg={12}>
                    {/*<AppBar position="relative" color="inherit">*/}
                        <Toolbar>
                            <ToggleButtonGroup
                                value={this.state.mode}
                                exclusive
                                onChange={this.changeMode}>
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
                            <div className={this.props.classes.grow} />
                            <IconButton color="inherit" onClick={this.saveClicked}>
                                <SaveIcon size="large"/>
                            </IconButton>
                        </Toolbar>
                    {/*</AppBar>*/}
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.classes.codeEditor}
                            value={this.state.value}
                            options={{
                                mode: 'smalltalk',
                                theme: 'material',
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
                            onBeforeChange={(editor, data, value) => {this.changed(value)}}
                            onChange={(editor, data, value) => {this.changed(value)}}
                        />
                    </Paper>
                </Grid>
            </Grid>
        )
    }
};

export default CodeEditor;

