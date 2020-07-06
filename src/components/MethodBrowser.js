import React, { Component } from 'react';
import { Grid, Paper, RadioGroup, FormControlLabel, Radio } from '@material-ui/core';
import axios from 'axios';
import clsx from 'clsx';

import SelectorList from './SelectorList';
import CodeEditor from './CodeEditor';

class MethodBrowser extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            selectedSelector: null,
            selectedMethod: {selector: 'selector', source: '"no source"'},
            side: "instance"
        }
    }

    selectorSelected = (s) => {
        this.setState({selectedSelector: s}, () => this.getMethod());
    }

    getDefinition = () => {
        const { classes, selectedClass } = this.state;
        if (classes[selectedClass] == null) { classes[selectedClass] = {name: selectedClass} }
        if (classes[selectedClass].definition == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass)
                .then(res => {
                    classes[selectedClass].definition = res.data;
                    this.setState({classes: classes})})
                .catch(error => {this.reportError(error)})
        }
    }

    getCommment = () => {        
        const { classes, selectedClass } = this.state;
        if (classes[selectedClass] == null) { classes[selectedClass] = {name: selectedClass} }
        if (classes[selectedClass].comment == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/comment')
                .then(res => {
                    classes[selectedClass].comment = res.data;
                    this.setState({classes: classes})})
                .catch(error => {this.reportError(error)})
        }
    }

    getMethod = () => {
        const { selectedClass, selectedSelector } = this.state;
        if (selectedClass == null || selectedSelector == null) { return };
        axios.get(this.props.baseUri + '/classes/' + selectedClass + '/methods/' + selectedSelector)
            .then(res => {this.setState({selectedMethod: res.data})})
            .catch(error => {this.reportError(error)})
    }

    currentDefinition() {
        const { classes, selectedClass } = this.state;
        if (selectedClass == null || classes[selectedClass] == null) { return '' };
        return classes[selectedClass].definition;
    }

    currentComment() {
        const { classes, selectedClass } = this.state;
        if (selectedClass == null || classes[selectedClass] == null) { return '' };
        return classes[selectedClass].comment;
    }

    changeSide = (e, side) => {
        if (side == null) return;
        this.setState({side: side});
        if (side === "instance") {
            const name = this.state.root;
            this.changeRoot(name.slice(0, name.length - 6))
        } else {
            this.changeRoot(this.state.root + " class")
        }
    }

    render() {
        const { 
            selectedSelector,
            selectedMethod } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <SelectorList
                            selectors={this.props.selectors}
                            onSelect={this.selectorSelected}/>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeEditor
                            baseUri={this.props.baseUri}
                            class={this.currentClass()}
                            definition={this.currentDefinition()}
                            comment={this.currentComment()}
                            category={selectedCategory}
                            selector={selectedSelector}
                            source={selectedMethod == null ? '' : selectedMethod.source}
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