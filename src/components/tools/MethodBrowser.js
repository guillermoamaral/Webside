import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import clsx from 'clsx';
import { AppContext } from '../../AppContext';
import MethodList from '../parts/MethodList';
import CodeEditor from '../parts/CodeEditor';

class MethodBrowser extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            selectedMethod: null,
            selectedMode: null,
        }
    }

    methodSelected = async (method) => {
        await this.updateClass(method);
        this.setState({selectedMethod: method, selectedMode: "source"});
    }

    updateClass = async (method) => {
        if (!method.classDefinition) {
            const definition = await this.context.api.getClass(method.class);
            method.classDefinition = definition.definition;
            method.classComment = definition.comment;
        }
    }

    defineClass = async (definition) => {
        const selected = this.state.selectedMethod;
        const species = await this.context.api.defineClass(selected.class, definition);   
        selected.classDefinition = species.definition; 
    }

    commentClass = async (comment) => {
        const selected = this.state.selectedMethod;
        const species = await this.context.api.commentClass(selected.class, comment);   
        selected.classComment = species.comment; 
    }

    compileMethod = async (source) => {
        const selected = this.state.selectedMethod;
        const method = await this.context.api.compileMethod(selected.class, selected.category, source);
        if (method.selector === selected.selector) {
            selected.source = method.source;
            this.setState({selectedMethod: selected})
        }
    }

    currentSource = () => {
        const {selectedMethod, selectedMode} = this.state;
        if (!selectedMethod) {return ''}
        let source;
        switch (selectedMode) {
            case "comment":
                source = selectedMethod.classComment;
                break;
            case "definition":
                source = selectedMethod.classDefinition;
                break;
            case "source":    
                source = selectedMethod.source;
                break;
            default:
        }
        return source
    }

    modeChanged = (event, mode) => {
        this.setState({selectedMode: mode})
    }

    saveClicked = (source) => {
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
        const {selectedMethod, selectedMode} = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <MethodList
                            showClass={true}
                            selectedMethod={selectedMethod}
                            methods={this.props.methods}
                            onSelect={this.methodSelected}/>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={12} lg={12}>
                            <ToggleButtonGroup
                                label="primary"
                                value={selectedMode}
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
                                classes={this.props.classes}
                                source={this.currentSource()}
                                showAccept
                                onAccept={this.saveClicked}/>
                        </Grid>
                    </Grid>
                </Grid>
            </Grid>
        )
    }
}

export default MethodBrowser;