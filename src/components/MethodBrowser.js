import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';

import SelectorList from './SelectorList';
import CodeEditor from './CodeEditor';

class MethodBrowser extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            selectedMethod: {selector: 'selector', source: '"no source"'},
        }
    }

    methodSelected = (method) => {
        this.setState({selectedMethod: method}, () => {
            this.updateClassDefinition();
        });
    }

    updateClassDefinition = () => {
        const method = this.state.selectedMethod;
        if (method.classDefinition == null) {
            this.props.api.definitionOf(method.class)
                .then(definition => {
                    method.classDefinition = definition.definitionString;
                    method.classComment = definition.comment;
                    this.setState({selectedMethod: method})})
                .catch(error => {})
        }
    }

    render() {
        const method = this.state.selectedMethod;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <SelectorList
                            api={this.props.api}
                            globalOptions={this.props.globalOptions}
                            showClass={true}
                            selectors={this.props.methods}
                            onSelect={this.methodSelected}/>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeEditor
                            classes={this.props.classes}
                            baseUri={this.props.baseUri}
                            class={method == null ? '' : method.class}
                            definition={method == null ? '' : method.classDefinition}
                            comment={method == null ? '' : method.classComment}
                            category={method == null ? '' : method.category}
                            selector={method == null ? '' : method.selector}
                            source={method == null ? '' : method.source}
                            onError={this.reportError}
                            onClassDefined={this.classDefined}
                            onClassCommented={this.classCommented}
                            onMethodCompiled={this.methodCompiled}
                            />
                    </Paper>
                </Grid> 
            </Grid>
        )
    };
}

export default MethodBrowser;