import React, { Component } from 'react';
import { Grid, Paper, Toolbar, IconButton } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import SaveIcon from '@material-ui/icons/Save';
import { Controlled as CodeMirror } from 'react-codemirror2';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class CodeEditor extends Component {
    constructor(props){
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            class: props.class,
            definition: props.definition,
            comment: props.comment,
            category: props.category,
            selector: props.selector,
            source: props.source,
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

    valueChanged = (value) => {
        this.setState({value: value, dirty: true})
    }

    modeChanged = (event, mode) => {
        this.setState({mode: mode, value: this.state[mode]})
    }

    defineClass = (definitionString) => {
        this.props.api.defineClass(this.props.class, definitionString)
            .then(definition => {
                this.setState({definition: definition.definitionString});
                if (this.props.onClassDefined !== undefined) {
                    const handler = this.props.onClassDefined;
                    handler(definition)
                }
            })
            .catch(error => {})
    }
    
    commentClass = (comment) => {
        this.props.api.commentClass(this.props.class, comment)
            .then(definition => {
                this.setState({comment: definition.comment});
                if (this.props.onClassCommented !== undefined) {
                    const handler = this.props.onClassCommented;
                    handler.bind(this);
                    handler(definition)
                }
            })
            .catch(error => {})
    }

    compileMethod = (source) => {
        this.props.api.compileMethod(this.props.class, this.props.category, source)
            .then(method => {
                this.setState({source: method.source, selector: method.selector})
                if (this.props.onMethodCompiled !== undefined) {
                    const handler = this.props.onMethodCompiled;
                    handler.bind(this);
                    handler(method)
                }})
            .catch(error => {})
    }
    
    saveClicked = (event) => {
        const value = this.state.value;
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
            </Grid>
        )
    }
};

export default CodeEditor;

