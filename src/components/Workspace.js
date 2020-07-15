import React, { Component } from 'react';
import {
    Grid,
    Paper,
    Toolbar,
    IconButton,
    Accordion,
    AccordionSummary,
    AccordionDetails,
    Typography
} from '@material-ui/core';
import PlayIcon from '@material-ui/icons/PlayArrow';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import InspectorIcon from '../Icons/InspectorIcon';
import { Controlled as CodeMirror } from 'react-codemirror2';

import Inspector from './Inspector';

require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class Workspace extends Component {
    constructor(props) {
        super(props);
        this.reportError=props.onError.bind();
        this.state = {
            expression: '1 @ 2 extent: 10',
            opensInspector: true,
            inspectors: [],
        };
    }

    openInspector(object) {
        const inspector = <Inspector
          api={this.props.api}
          key={object.id}
          classes={this.props.classes}
          root={object}
          onClose={this.closeInspector}
          onError={this.reportError}
          />;
        const inspectors = this.state.inspectors;
        inspectors.unshift(inspector);
        this.setState({inspectors: inspectors})
    }

    closeInspector = (id) => {
        this.setState({inspectors: this.state.inspectors.filter((i) => {return i.props.root.id !== id})});
    }
    
    expressionChanged(text) {
        this.setState({expression: text})
    }

    evaluateClicked = () => {
        this.props.api.evaluate(this.state.expression, true)
            .then(object => {
                if (this.state.opensInspector) {
                    this.openInspector(object)
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
            <Grid container spacing={1}>
                <Grid item xs={12} md={8} lg={8}>
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
                                    matchTags: {bothTags: true}, 
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
                <Grid item xs={12} md={4} lg={4}>
                    {this.state.inspectors.map((inspector, index) => {
                        return (
                            <Accordion key={inspector.key} defaultExpanded={index===0}>
                                <AccordionSummary
                                    expandIcon={<ExpandMoreIcon />}
                                    id="panel1a-header"
                                    >
                                    <InspectorIcon/>
                                    <Typography>
                                        {inspector.props.root.class + ': ' + inspector.props.root.id}
                                    </Typography>
                                </AccordionSummary>
                                {inspector}
                            </Accordion>
                        )         
                    })}
                </Grid>
            </Grid>
        )
    }
}

export default Workspace;
