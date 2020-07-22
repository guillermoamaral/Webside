import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import { Controlled as CodeMirror } from 'react-codemirror2';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class Transcript extends Component {
    constructor(props) {
        super(props);
        this.state = {
            text: props.text,
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.text !== state.text) {
            return {
                text: props.text
            };
        }
        return null
    }

    render() {
        return (
            <Grid container>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            style={{height: 100}}
                            className={this.props.classes.codeMirror}
                            value={this.props.text}
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
                                lineWrapping: true}}
                        />
                    </Paper>
                </Grid>
            </Grid>
        )
    }
}

export default Transcript;
