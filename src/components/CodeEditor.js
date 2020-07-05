import React, { Component } from 'react';
import { Grid, Paper, AppBar, Toolbar, IconButton } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import SaveIcon from '@material-ui/icons/Save';
import { withStyles } from '@material-ui/core/styles';
import { Controlled as CodeMirror } from 'react-codemirror2';
import axios from 'axios';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

const styles = (theme) => ({
    root: {
        //fontFamily: Inconsolata, monospace;
        fontSize: 16,
        backgroundColor: theme.palette.background.paper
        //height: "100%",
    },
    grow: {
        flexGrow: 1
    },
  });

class CodeEditor extends Component {
    constructor(props){
        console.log('initializing')
        super(props);
        this.state = {
            class: '"no class"',
            definition: '"no class definition"', 
            comment: '"no class comment"',
            method: '"no method code"',
            value: '',
            mode: "method",
            dirty: false
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.class !== state.class ||
            props.definition !== state.definition ||
            props.comment !== state.comment ||
            props.method !== state.method) {
            return {
                class: props.class,
                definition: props.definition,
                comment: props.comment,
                method: props.method,
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

    postDefinition = (definition) => {
        axios.post(this.props.baseUri + '/classes', definition)
            .then(res => {
                this.setState({definition: definition})
            })
    }
    
    postCommment = (comment) => {
        axios.post(this.props.baseUri + '/classes/' + this.props.class + '/comment', comment)
            .then(res => {
                this.setState({comment: comment})
            })
    }

    postMethod = (source) => {
        const method = {source: source};
        axios.post(this.props.baseUri + '/classes/' + this.props.class + '/methods', method)
            .then(res => {
                this.setState({method: res.data})
            })
    }

    saveClicked = (e) => {
        const { value } = this.state;
        switch (this.state.mode) { 
            case "comment":
                this.postComment(value);
                break;
            case "definition":
                this.postDefinition(value);
                break;
            case "method":    
                this.postMethod(value);
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
                                <ToggleButton value="method" variant="outlined" size="small">
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
                                <SaveIcon />
                            </IconButton>
                        </Toolbar>
                    {/*</AppBar>*/}
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.classes.root}
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

export default withStyles(styles)(CodeEditor);

