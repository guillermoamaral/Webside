import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';

import MethodList from './MethodList';
import CodeEditor from './CodeEditor';

class MethodBrowser extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            selectedMethod: null,
        }
    }

    methodSelected = (method) => {
        this.updateClass(method);
        this.setState({selectedMethod: method});
    }

    updateClass = (method) => {
        if (method.classDefinition === undefined) {
            this.props.api.getClass(method.class)
                .then(definition => {
                    method.classDefinition = definition.definition;
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
                        <MethodList
                            api={this.props.api}
                            globalOptions={this.props.globalOptions}
                            showClass={true}
                            selectedMethod={method}
                            methods={this.props.methods}
                            onSelect={this.methodSelected}/>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeEditor
                            classes={this.props.classes}
                            baseUri={this.props.baseUri}
                            definition={method == null ? '' : method.classDefinition}
                            comment={method == null ? '' : method.classComment}
                            source={method == null ? '' : method.source}
                            onError={this.reportError}
                            onDefine={this.defineClass}
                            onComment={this.commentClass}
                            onCompile={this.compileMethod}
                            />
                    </Paper>
                </Grid> 
            </Grid>
        )
    };
}

export default MethodBrowser;