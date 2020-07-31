import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';
import { AppContext } from '../../AppContext';
import MethodList from '../parts/MethodList';
import CodeBrowser from '../parts/CodeBrowser';

class MethodBrowser extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            selectedMethod: null,
            selectedClass: null,
        }
    }

    methodSelected = async (method) => {
        const species = await this.context.api.getClass(method.class);
        this.setState({selectedMethod: method, selectedClass: species});
    }

    defineClass = async (definition) => {
        const species = await this.context.api.defineClass(this.state.selectedClass, definition);
    }

    commentClass = async (comment) => {
        const species = await this.context.api.commentClass(this.state.selectedClass, comment);   
    }

    compileMethod = async (source) => {
        const selected = this.state.selectedMethod;
        const method = await this.context.api.compileMethod(selected.class, selected.category, source);
        if (method.selector === selected.selector) {
            selected.source = method.source;
            this.setState({selectedMethod: selected})
        }
    }

    render() {
        const {selectedMethod, selectedClass} = this.state;
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
                    <CodeBrowser
                        classes={this.props.classes}
                        class={selectedClass}
                        method={selectedMethod}
                        onCompileMethod={this.compileMethod}
                        onDefineClass={this.defineClass}
                        onCommentClass={this.commentClass}/>
                </Grid>
            </Grid>
        )
    }
}

export default MethodBrowser;