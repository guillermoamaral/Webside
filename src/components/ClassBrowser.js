import React, { Component } from 'react';
import { Grid, Paper, RadioGroup, FormControlLabel, Radio } from '@material-ui/core';
import axios from 'axios';
import clsx from 'clsx';

import SearchList from './SearchList';
import ClassTree from './ClassTree';
import SelectorList from './SelectorList';
import SimpleList from './SimpleList';
import CodeEditor from './CodeEditor';

class ClassBrowser extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            root: this.props.root,
            classTree: [],
            classes: {},
            classNames: [],
            selectedClass: null,
            selectedVariable: null,
            selectedCategory: null,
            selectedSelector: null,
            selectedMethod: {selector: 'selector', source: '"no source"'},
            side: "instance"
        }
    }

    componentDidMount(){
        this.changeRoot(this.state.root)
    }

    changeRoot = (root) => {
        const selected = this.state.selectedClass !== null;
        this.setState({root: root}, () => {
            this.getClassTree();
            this.getClassNames();
            if (selected) { this.classSelected(root) };
        })
    }

    classSelected = (c) => {
        this.setState({selectedClass: c}, () => {
            this.getDefinition();
            this.getCommment();
            this.getVariables();
            this.getCategories()
        })
    }

    variableSelected = (v) => {
        this.setState({selectedVariable: v});
    }

    categorySelected = (c) => {
        this.setState({selectedCategory: c}, () => this.getSelectors());
    }

    selectorSelected = (s) => {
        this.setState({selectedSelector: s}, () => this.getMethod());
    }

    getClassTree = () => {
        axios.get(this.props.baseUri + '/classes?root=' + this.state.root + '&tree=true')
            .then(res => {this.setState({classTree: res.data})})
            .catch(error => {this.reportError(error)})
    }

    getClassNames = () => {
        axios.get(this.props.baseUri + '/classes?names=true')
            .then(res => {this.setState({classNames: res.data})})
            .catch(error => {this.reportError(error)})
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

    getVariables = () => {
        const { classes, selectedClass, selectedVariable } = this.state;
        if (classes[selectedClass] == null) { classes[selectedClass] = {name: selectedClass} }
        if (classes[selectedClass].variables == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/variables')
                .then(res => {
                    classes[selectedClass].variables = res.data;

                    var selected = selectedVariable;
                    if (!classes[selectedClass].variables.includes(selected)) {
                        selected = null;
                    }
                    this.setState({classes: classes, selectedVariable: selected})})
                .catch(error => {this.reportError(error)})
        }
    }

    getCategories = () => {
        const { classes, selectedClass, selectedCategory } = this.state;
        if (classes[selectedClass] == null) { classes[selectedClass] = {name: selectedClass} }
        if (classes[selectedClass].categories == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/categories')
                .then(res => {
                    classes[selectedClass].categories = res.data;
                    var selected = selectedCategory;
                    if (!classes[selectedClass].categories.includes(selected)) {
                        selected = null;
                    }
                    this.setState({classes: classes, selectedCategory: selected})})
                .catch(error => {this.reportError(error)})
        }
    }

    getSelectors = () => {
        const { classes, selectedClass, selectedCategory } = this.state;
        if (selectedClass !== null && selectedCategory !== null 
            && (classes[selectedClass].selectors == null || classes[selectedClass].selectors[selectedCategory] == null)) { 
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/selectors?category=' + selectedCategory + '&marks=true')
                .then(res => {
                    if (classes[selectedClass].selectors == null) {
                        classes[selectedClass].selectors = {}
                    };
                    const sorted = res.data.sort(function(a, b){ return a.selector <= b.selector? -1 : 1 });
                    classes[selectedClass].selectors[selectedCategory] = sorted;
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

    currentVariables() {
        const { classes, selectedClass } = this.state;
        if (selectedClass == null || classes[selectedClass] == null) { return [] };
        return classes[selectedClass].variables;
    }

    currentCategories = () => {
        const { classes, selectedClass } = this.state;
        if (selectedClass == null || classes[selectedClass] == null) { return [] };
        return classes[selectedClass].categories;
    }

    currentSelectors = () => {
        const { classes, selectedClass, selectedCategory } = this.state;
        if (selectedClass == null || selectedCategory == null
                || classes[selectedClass] == null
                || classes[selectedClass].selectors == null) { return [] };
        return classes[selectedClass].selectors[selectedCategory];
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

    methodCompiled = (method) => {
        this.categorySelected(method.category);
        this.selectorSelected(method.selector);
    }

    render() {
        const { 
            classes,
            classTree,
            selectedClass,
            selectedVariable,
            selectedCategory,
            selectedSelector,
            selectedMethod } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={6} lg={6}>
                    <Paper>
                        {/*<SearchList options={classes}/>*/}
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <ClassTree
                                    classes={classTree}
                                    onSelect={this.classSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <SimpleList
                                    items={this.currentVariables()}
                                    selectedItem={selectedVariable}
                                    onSelect={this.variableSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <SimpleList
                                    items={this.currentCategories()}
                                    selectedItem={selectedCategory}
                                    onSelect={this.categorySelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <SelectorList
                                    selectors={this.currentSelectors()}
                                    onSelect={this.selectorSelected}/>
                            </Paper>
                        </Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1} justify="center">
                        <Grid item xs={12} md={6} lg={6}></Grid>
                        <Grid item xs={12} md={6} lg={6}>
                            <RadioGroup row name="side" value={this.state.side} onChange={this.changeSide} defaultValue="instance" size="small">
                                <FormControlLabel value="instance" control={<Radio size="small" color="default"/>} label="Instance"/>
                                <FormControlLabel value="class" control={<Radio size="small" color="default"/>} label="Class" />
                            </RadioGroup>
                        </Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeEditor
                            baseUri={this.props.baseUri}
                            class={selectedClass}
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

export default ClassBrowser
