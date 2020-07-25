import React, { Component } from 'react';
import { Grid, Paper, Box, IconButton } from '@material-ui/core';
import AcceptIcon from '@material-ui/icons/CheckCircle';
import { Controlled as CodeMirror } from 'react-codemirror2';
import PopupMenu from '../controls/PopupMenu';
import { AppContext } from '../../AppContext';
require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');

class CodeEditor extends Component {
    static contextType = AppContext;

    constructor(props){
        super(props);
        this.editor = null;
        this.state = {
            source: props.source,
            dirty: false,
            value: props.source,
            selectedRanges: [],
            menuOpen: false,
            menuPosition: {x: null, y: null}
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.source !== state.source 
            || (props.selectedRanges !== undefined && props.selectedRanges !== state.selectedRanges)) {
            return {
                source: props.source,
                value: props.source,
                selectedRanges: props.selectedRanges === undefined ? [] : props.selectedRanges,
            }
        };
        return null;
    }

    selectRange(range) {
        this.selectRanges([range]);
    }

    selectRanges(ranges){
        if (ranges.length > 0) {
            const selections = ranges.map(r => {
                return {anchor: this.lineChAt(r.start), head: this.lineChAt(r.end + 1)}
            });
            this.editor.setSelections(selections)
        }
    }
    
    lineChAt(index) {
        var lines = this.state.value.slice(0, index).split("\r");
		return {line: lines.length - 1, ch: lines[lines.length - 1].length - 1};
	}

    openMenu = (event) => {
        event.preventDefault();
        this.setState({menuOpen: true, menuPosition: {x: event.clientX - 2, y: event.clientY - 4}})
      };
    
    closeMenu = () => {
        this.setState({menuOpen: false});
    }
    
    menuOptionClicked(option) {
        if (option.action !== undefined) {
          option.action(this.state.selectedItem);
        }
    }

    menuOptions() {
        const options = [
            {label: 'Do it', action: this.evaluate},
            {label: 'Show it', action: this.show},
            {label: 'Inspect it', action: this.inspect},
            null,
            {label: 'Senders', action: this.browseSenders},
            {label: 'Implementors', action: this.browseImplementors},
            {label: 'Class references', action: this.browseReferences}
        ];
        return options;
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
        const selector = this.editor.getSelection();
        this.context.browseSenders(selector);
    }

    browseImplementors = () => {
        const selector = this.editor.getSelection();
        this.context.browseImplementors(selector);
    }

    browseClass = () => {
        const classname = this.editor.getSelection();
        this.context.browseClass(classname);
    }

    browseReferences = () => {
        const global = this.editor.getSelection();
        this.context.browseReferences(global);
    }

    evaluableExpression() {
        const expression = this.editor.getSelection();
        if (expression.length > 0) {return expression}
        const cursor = this.editor.getCursor("to");
        return this.editor.getRange({ch: 0, line: cursor.line}, cursor)
    }

    evaluate = async () => {
        const expression = this.evaluableExpression();
        try {
            await this.context.evaluateExpression(expression, false);
        }
        catch (error) {}
    }

    show = async () => {
        const expression = this.evaluableExpression();
        try {
            const object = await this.context.evaluateExpression(expression, false);
            const cursor = this.editor.getCursor("to");
            this.editor.replaceRange(" " + object.printString, cursor);
            const from = {ch: cursor.ch + 1, line: cursor.line};
            const to = {ch: from.ch + object.printString.length, line: from.line};
            this.editor.setSelection(from, to)
        }
        catch (error) {}
    }

    inspect = async () => {
        const expression = this.evaluableExpression();
        try {
            const object = await this.context.evaluateExpression(expression, true);
            this.context.inspectObject(object);          
        }
        catch (error) {}
    }
  
    render() {
        const showAccept = this.props.showAccept;
        const ranges = this.state.selectedRanges; 
        if (ranges.length > 0) {this.selectRanges(ranges)}
        return (
            <Grid container spacing={1}>
                <Grid item xs={11} md={showAccept? 11 : 12} lg={showAccept? 11 : 12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.classes.codeMirror}
                            options={{
                                //viewportMargin: "Infinity",
                                mode: "smalltalk",
                                theme: "material",
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
                                    "Ctrl-u": this.debug,
                                    "Alt-S": this.acceptClicked,
                                    "Ctrl-B": this.browseClass,
                                    "Alt-N": this.browseSenders,
                                    "Alt-M": this.browseImplementors,
                                    "Alt-R": this.browseReferences
                                }}}
                            value={this.state.value}
                            editorDidMount={editor => {this.editor = editor; editor.setSize("100%", "100%")}}
                            onBeforeChange={(editor, data, value) => {this.valueChanged(value)}}
                            onChange={(editor, data, value) => {this.valueChanged(value)}}
                            onContextMenu={(editor, event) => {this.openMenu(event)}}
                        />
                    </Paper>
                </Grid>
                {showAccept && (<Grid item xs={1} md={1} lg={1}>
                    <Box display="flex" justifyContent="center" > 
                        <IconButton color="inherit" onClick={this.acceptClicked}>
                            <AcceptIcon size="large" style={{fontSize: 30}}/>
                        </IconButton>
                    </Box>
                </Grid>)}
                <PopupMenu
                    options={this.menuOptions()}
                    open={this.state.menuOpen}
                    position={this.state.menuPosition}
                    onClose={this.closeMenu}/>
            </Grid>
        )
    }
};

export default CodeEditor;

