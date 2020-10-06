import React, { Component } from 'react';
import { Grid, Paper,LinearProgress } from '@material-ui/core';
import { Controlled as CodeMirror } from 'react-codemirror2';
import PopupMenu from '../controls/PopupMenu';
require('codemirror/lib/codemirror.css');
require('codemirror/theme/material.css');
require('codemirror/mode/gas/gas.js');
require('codemirror/addon/search/searchcursor.js');
require('codemirror/addon/search/search.js');
require('codemirror/addon/search/jump-to-line.js');
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

class NativeCodeEditor extends Component {
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
            }
        }
        return null;
    }

    editorDidMount(editor) {
        this.editor = editor; 
        this.editor.setSize("100%", "100%");
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
    }
    
    closeMenu = () => {
        this.setState({menuOpen: false});
    }

    menuOptions() {
        return [
        ]
    }

    valueChanged = (value) => {
        const handler = this.props.onChange;
        if (handler) {handler(value)}
        const ranges = value === this.state.value? this.state.ranges : null;
        this.setState({value: value, dirty: true, ranges: ranges})
    }

    toggleFullScreen = (event) => {
        this.editor.setOption("fullScreen", !this.editor.getOption("fullScreen"));
    }

    lintAnnotations = ()  => {
        if (!this.props.lintAnnotations) {return []}
        return this.props.lintAnnotations.map(a => {
            return {
                from: this.lineChAt(a.from - 1),
                to: this.lineChAt(a.to - 1),
                severity: a.severity,
                message: a.description}
            }
        )
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
        const {value, ranges, evaluating} = this.state;
        if (ranges && ranges.length > 0) {this.selectRanges(ranges)}
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeMirror
                            className={this.props.styles.codeMirror}
                            options={{
                                mode: "gas",
                                theme: "material",
                                lineSeparator: '\r',
                                lineNumbers: this.props.lineNumbers,
                                matchBrackets: true,
                                autoCloseBrackets: true,
                                highlightSelectionMatches: {annotateScrollbar: true},
                                indentUnit: 10, 
                                styleActiveLine: true, 
                                matchTags: {bothTags: true}, 
                                lineWrapping: true,
                                gutters: ['CodeMirror-lint-markers', 'breakpoints'],
                                lint: {'getAnnotations': this.lintAnnotations},
                                extraKeys: {
                                    "Alt-Z": this.toggleFullScreen}}}
                            value={value}
                            editorDidMount={editor => {this.editorDidMount(editor)}}
                            onGutterClick={(editor, n) => {this.setBreakpoint(n)}}
                            onBeforeChange={(editor, data, value) => {this.valueChanged(value)}}
                            onChange={(editor, data, value) => {this.valueChanged(value)}}
                            onContextMenu={(editor, event) => {this.openMenu(event)}}/>
                            {evaluating && <LinearProgress variant="indeterminate"/>}
                    </Paper>
                </Grid>
                <PopupMenu
                    options={this.menuOptions()}
                    open={this.state.menuOpen}
                    position={this.state.menuPosition}
                    onClose={this.closeMenu}/>
            </Grid>
        )
    }
}

export default NativeCodeEditor;