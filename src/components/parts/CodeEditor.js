import React, { Component } from 'react';
import { Grid, Paper, Box, IconButton, LinearProgress } from '@material-ui/core';
import AcceptIcon from '@material-ui/icons/CheckCircle';
import { Controlled as CodeMirror } from 'react-codemirror2';
import PopupMenu from '../controls/PopupMenu';
import { AppContext } from '../../AppContext';
require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/smalltalk/smalltalk.js');
require('codemirror/addon/search/searchcursor.js');
require('codemirror/addon/search/search.js');
require('codemirror/addon/search/match-highlighter.js');
require('codemirror/addon/edit/matchbrackets.js');
require('codemirror/addon/edit/closebrackets.js');
require('codemirror/addon/comment/comment.js');
require('codemirror/addon/selection/active-line.js');
require('codemirror/addon/display/fullscreen.js');
require('codemirror/addon/display/fullscreen.css');
require('codemirror/addon/scroll/annotatescrollbar.js');
require('codemirror/addon/lint/lint.js');
require('codemirror/addon/lint/lint.css');

class CodeEditor extends Component {
    static contextType = AppContext;

    constructor(props){
        super(props);
        this.editor = null;
        this.state = {
            source: props.source,
            selectedRanges: [],
            dirty: false,
            value: props.source,
            ranges: [],
            menuOpen: false,
            menuPosition: {x: null, y: null},
            evaluating: false,
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.source !== state.source 
            || (props.selectedRanges && props.selectedRanges !== state.selectedRanges)) {
            return {
                source: props.source,
                selectedRanges: props.selectedRanges,
                value: props.source,
                ranges: props.selectedRanges,
                evaluating: props.evaluating,
            }
        }
        if (props.evaluating !== state.evaluating) {
            return {
                evaluating: props.evaluating,
            }
        };
        return null;
    }

    editorDidMount(editor) {
        this.editor = editor; 
        this.editor.setSize("100%", "100%");
    }
    
    markOcurrences = () => {
        const keyword = this.editor.getSelection();
        var cursor = this.editor.getSearchCursor(keyword);
        while (cursor.findNext()) {
            this.editor.markText(
              cursor.from(),
              cursor.to(),
              {className: 'highlight'}
            )
        }
      }

    selectRange(range) {
        this.selectRanges([range]);
    }

    selectRanges(ranges){
        if (ranges.length > 0) {
            const selections = ranges.map(r => {
                return {anchor: this.lineChAt(r.start - 1), head: this.lineChAt(r.end)}
            });
            this.editor.setSelections(selections)
        }
    }
    
    lineChAt(index) {
        var lines = this.state.value.slice(0, index).split("\r");
		return {line: lines.length - 1, ch: lines[lines.length - 1].length};
	}

    openMenu = (event) => {
        event.preventDefault();
        this.setState({menuOpen: true, menuPosition: {x: event.clientX - 2, y: event.clientY - 4}})
      };
    
    closeMenu = () => {
        this.setState({menuOpen: false});
    }
    
    menuOptionClicked(option) {
        const selected = this.state.selectedItem;
        if (option.action) {
            option.action(selected)
        }
    }

    menuOptions() {
        return [
            {label: 'Do it', action: this.evaluateExpression},
            {label: 'Show it', action: this.showEvaluation},
            {label: 'Inspect it', action: this.inspectEvaluation},
            {label: 'Debug it', action: this.debugExpression},
            null,
            {label: 'Browse', action: this.browseClass},
            {label: 'Senders', action: this.browseSenders},
            {label: 'Implementors', action: this.browseImplementors},
            {label: 'Class references', action: this.browseReferences}
        ]
    }

    valueChanged = (value) => {
        const handler = this.props.onChange;
        if (handler) {handler(value)}
        const ranges = value === this.state.value? this.state.ranges : null;
        this.setState({value: value, dirty: true, ranges: ranges})
    }
    
    acceptClicked = (event) => {
        const handler = this.props.onAccept;
        if (handler) {handler(this.state.value)}
    }

    toggleFullScreen = (event) => {
        this.editor.setOption("fullScreen", !this.editor.getOption("fullScreen"));
    }

    targetWord() {
        const word = this.editor.getSelection();
        if (word.length > 0) {return word}
        const stretch = this.editor.findWordAt(this.editor.getCursor());
        return this.editor.getRange(stretch.anchor, stretch.head);    
    }

    browseSenders = () => {
        this.context.browseSenders(this.targetWord());
    }

    browseImplementors = () => {
        this.context.browseImplementors(this.targetWord());
    }

    browseClass = () => {
        this.context.browseClass(this.targetWord());
    }

    browseReferences = () => {
        this.context.browseReferences(this.targetWord());
    }

    selectedExpression() {
        const expression = this.editor.getSelection();
        if (expression.length > 0) {return expression}
        const cursor = this.editor.getCursor();
        return this.editor.getLine(cursor.line);
    }

    debugExpression = async () => {
        const expression = this.selectedExpression();
        try {
            await this.context.debugExpression(expression);
        }
        catch (error) {}
    }

    evaluateExpression = async () => {
        const expression = this.selectedExpression();
        try {
            this.setState({evaluating: true});
            await this.context.evaluateExpression(expression, false);
            this.setState({evaluating: false});
        }
        catch (error) {this.setState({evaluating: false})}
    }

    showEvaluation = async () => {
        const expression = this.selectedExpression();
        try {
            this.setState({evaluating: true});
            const object = await this.context.evaluateExpression(expression, false);
            this.setState({evaluating: false});
            const cursor = this.editor.getCursor("to");
            if (this.editor.getSelection().length === 0) {
                cursor.ch = this.editor.getLine(cursor.line).length;
            }
            this.editor.replaceRange(" " + object.printString, cursor);
            const from = {ch: cursor.ch + 1, line: cursor.line};
            const to = {ch: from.ch + object.printString.length, line: from.line};
            this.editor.setSelection(from, to);
        }
        catch (error) {this.setState({evaluating: false})}
    }

    inspectEvaluation = async () => {
        const expression = this.selectedExpression();
        try {
            this.setState({evaluating: true});
            const object = await this.context.evaluateExpression(expression, true);
            this.setState({evaluating: false});
            this.context.inspectObject(object);          
        }
        catch (error) {this.setState({evaluating: false})}
    }

    lintAnnotations = ()  => {
        if (!this.props.lintAnnotations) {return []}
        return this.props.lintAnnotations.map(a => {
            return {
                from: this.lineChAt(a.from - 1),
                to: this.lineChAt(a.to - 1),
                severity: a.severity,
                message: a.description}
            })
    }

    setBreakpoint = (n) => {
        var info = this.editor.lineInfo(n);
        this.editor.setGutterMarker(n, "breakpoints", info.gutterMarkers ? null : this.makeMarker());
    }
      
    makeMarker() {
        var marker = document.createElement('div');
        marker.style.color = 'red';
        marker.innerHTML = "●";
        return marker;
      }
  
    render() {
        const showAccept = this.props.showAccept;
        const acceptIcon = this.props.acceptIcon?
            React.cloneElement(this.props.acceptIcon)
            : <AcceptIcon size="large" style={{fontSize: 30}}/>;
        const ranges = this.state.ranges;
        if (ranges && ranges.length > 0) {this.selectRanges(ranges)}
        return (
            <Grid container spacing={1}>
                <Grid item xs={11} md={showAccept? 11 : 12} lg={showAccept? 11 : 12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.styles.codeMirror}
                            options={{
                                //viewportMargin: "Infinity",
                                mode: "smalltalk",
                                theme: "material",
                                lineSeparator: '\r',
                                lineNumbers: this.props.lineNumbers,
                                matchBrackets: true,
                                autoCloseBrackets: true,
                                //highlightSelectionMatches: true,
                                highlightSelectionMatches: {annotateScrollbar: true},
                                indentUnit: 10, 
                                styleActiveLine: true, 
                                matchTags: {bothTags: true}, 
                                lineWrapping: true,
                                gutters: ['CodeMirror-lint-markers', 'breakpoints'],
                                lint: {'getAnnotations': this.lintAnnotations},
                                extraKeys: {
                                    "Ctrl-D": this.evaluateExpression,
                                    "Ctrl-I": this.inspectEvaluation,
                                    "Ctrl-S": this.showEvaluation,
                                    "Ctrl-P": this.showEvaluation,
                                    "Ctrl-U": this.debugExpression,
                                    "Alt-S": this.acceptClicked,
                                    "Ctrl-B": this.browseClass,
                                    "Alt-N": this.browseSenders,
                                    "Alt-M": this.browseImplementors,
                                    "Alt-R": this.browseReferences,
                                    "Ctrl-Q": this.markOcurrences,
                                    "Alt-Z": this.toggleFullScreen}}}
                            value={this.state.value}
                            editorDidMount={editor => {this.editorDidMount(editor)}}
                            onGutterClick={(editor, n) => {this.setBreakpoint(n)}}
                            onBeforeChange={(editor, data, value) => {this.valueChanged(value)}}
                            //onChange={(editor, data, value) => {this.valueChanged(value)}}
                            onContextMenu={(editor, event) => {this.openMenu(event)}}/>
                            {this.state.evaluating && <LinearProgress variant="indeterminate"/>}
                    </Paper>
                </Grid>
                {showAccept && (<Grid item xs={1} md={1} lg={1}>
                    <Box display="flex" justifyContent="center" > 
                        <IconButton color="inherit" onClick={this.acceptClicked}>
                            {acceptIcon}
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

