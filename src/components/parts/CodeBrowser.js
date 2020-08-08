import React, { Component } from 'react';
import { Grid } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import { AppContext } from '../../AppContext';
import CodeEditor from './CodeEditor';

class CodeBrowser extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            method: null,
            selectedMode: "source",
        }
    }

    static getDerivedStateFromProps(props, state) {
        const mode = !props.method? "definition" : !state.method? "source" : state.selectedMode;
        return {
            selectedMode: mode,
            method: props.method
        }
    }

    defineClass = async (definition) => {
        const handler = this.props.onDefineClass;
        if (handler) {handler(definition)}
    }

    commentClass = async (comment) => {
        const handler = this.props.onCommentClass;
        if (handler) {handler(comment)}
    }

    compileMethod = async (source) => {
        const handler = this.props.onCompileMethod;
        if (handler) {handler(source)}
    }

    currentSource = () => {
        const method = this.props.method;
        const species = this.props.class;
        const mode = this.state.selectedMode;
        let source;
        switch (mode) {
            case "comment":
                source = !species? '' : species.comment;
                break;
            case "definition":
                source = !species? '' : species.definition;
                break;
            case "source":    
                source = !method? '' : method.source;
                break;
            default:
        }
        return source;
    }

    currentAnnotations = () => {
        if (this.state.selectedMode === "source") {
            const method = this.props.method; 
            return method? Date(method.timestamp) + ' by ' + method.author : '';
        }
        return ''
    }

    currentLintAnnotations = () => {
        if (this.state.selectedMode === "source") {
            const method = this.props.method; 
            return method? method.lintAnnotations : [];
        }
        return ''
    }

    modeChanged = (event, mode) => {
        this.setState({selectedMode: mode})
    }

    acceptClicked = (source) => {
        switch (this.state.selectedMode) {
            case "comment":
                this.commentClass(source);
                break;
            case "definition":
                this.defineClass(source);
                break;
            case "source":    
                this.compileMethod(source);
                break;
            default:
        }
    }

    render() {
        const mode = this.state.selectedMode;
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <ToggleButtonGroup
                        label="primary"
                        value={mode}
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
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <CodeEditor
                        styles={this.props.styles}
                        lineNumbers={true}
                        source={this.currentSource()}
                        lintAnnotations={this.currentLintAnnotations()}
                        selectedRanges={!this.props.selectedInterval? [] : [this.props.selectedInterval]}
                        showAccept
                        onAccept={this.acceptClicked}/>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    {this.currentAnnotations()}
                </Grid>
            </Grid>
        )
    }
}

export default CodeBrowser;