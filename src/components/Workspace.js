import React, { Component } from 'react';
import { Grid, Paper, Toolbar, IconButton } from '@material-ui/core';
import { Controlled as CodeMirror } from 'react-codemirror2';
import PlayIcon from '@material-ui/icons/PlayArrow';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class Workspace extends Component {
    constructor(props) {
        super(props);
        if (props.onEvaluate !== undefined) {
            this.onEvaluate = (object) => { props.onEvaluate(object) };
        };
        this.state = {
            expression: ''
        };
    }
    expressionChanged(text) {
        this.setState({expression: text})
    }

    evaluateClicked = () => {
        this.props.api.evaluate(this.state.expression)
            .then(object => {
                if (this.onEvaluate !== undefined) {
                    this.onEvaluate(object)
                } else {
                    this.setState({expression: this.state.expression + ' -> ' + object.printString})
                }
            })
            .catch(error => {
                this.setState({expression: this.state.expression + ' -> ' + error})
            })
    }

    render() {
        return (
            <Grid container spacing={0}>
                <Grid item xs={12} md={12} lg={12}>
                    <Toolbar>
                        <div className={this.props.classes.grow} />
                        <IconButton color="inherit" onClick={this.evaluateClicked}>
                            <PlayIcon size="large"/>
                        </IconButton>
                    </Toolbar>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.classes.codeEditor}
                            value={this.state.expression}
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
                                    "Alt-I": "inspectClicked", 
                                    "Ctrl-P": "printClicked"
                                }}}
                            onBeforeChange={(editor, data, value) => {this.expressionChanged(value)}}
                            onChange={(editor, data, value) => {this.expressionChanged(value)}}
                        />
                    </Paper>
                </Grid>
            </Grid>
        )
    }
}

export default Workspace;
