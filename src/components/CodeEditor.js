import React, { Component } from 'react';
import { Grid, Paper, AppBar, Toolbar, IconButton } from '@material-ui/core';
import SaveIcon from '@material-ui/icons/Save';
import { withStyles } from '@material-ui/core/styles';
import { Controlled as CodeMirror } from 'react-codemirror2';
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
        super(props);
        this.state = {
            source: '"no source"',
            value: '"no source"',
            dirty: false
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.source !== state.source) {
            return {
                source: props.source,
                value: props.source
            }
        };
        return null;
    }

    changed = (value) => {
        this.setState({value: value, dirty: true})
    } 

    saveClicked = (e) => {
        if (this.props !== null) { 
          const handler = this.props.onSave;
          if (handler !== null) {
              handler.bind(this);
              handler(this.state.value);
              this.setState({dirty: false});
          }
        }
      };

    render() {
        return (
                <Grid container spacing={0}>
                    <Grid item xs={12} md={12} lg={12}>
                        <AppBar position="relative" color="inherit">
                            <Toolbar>
                                <div className={this.props.classes.grow} />
                                <IconButton color="inherit">
                                    <SaveIcon />
                                </IconButton>
                            </Toolbar>
                        </AppBar>
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

