import React, { Component } from "react";
import { Grid, Paper } from "@material-ui/core";
import axios from 'axios';
import clsx from "clsx";

import ClassTree from './ClassTree';
import SimpleList from './SimpleList';
import CodeEditor from './CodeEditor';

class ClassBrowser extends Component {
    constructor(props) {
        super(props);
        this.state = {
            root: this.props.root,
            classes: [],
            definitions: {},
            selectedClass: this.props.root,
            selectedVariable: null,
            selectedCategory: null,
            selectedSelector: null,
            variables: {},
            categories: {},
            selectors: {},
            selectedMethod: {selector: 'no method', source: 'nothing'}
        }
    }

    componentDidMount(){
        axios.get(this.props.baseUri+'/class-tree?root='+this.state.root)
            .then(res => {this.setState({classes: res.data})}
        )
    }

    classSelected = (c) => {
        this.setState({selectedClass: c}, () => {
            this.getDefinition();
            this.getVariables();
            this.getCategories();
            this.getSelectors();});
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

    getDefinition = () => {
        const c = this.state.selectedClass;
        if (this.state.definitions[c] == null) {
            axios.get(this.props.baseUri+'/classes/'+ c)
                .then(res => { this.setState({definitions: {[c]: res.data}}) })
        }
    }

    getVariables = () => {
        const c = this.state.selectedClass;
        if (this.state.variables[c] == null) {
            axios.get(this.props.baseUri+'/classes/'+ c + '/instance-variables')
                .then(res => { this.setState({variables: {[c]: res.data}}) })
        }
    }

    getCategories = () => {
        const c = this.state.selectedClass;
        if (this.state.categories[c] == null) {
            axios.get(this.props.baseUri+'/classes/'+ c + '/categories')
                .then(res => { this.setState({categories: {[c]: res.data.sort()}}) })
        }
    }

    getSelectors = () => {
        const c = this.state.selectedClass;
        const k = this.state.selectedCategory;
        if (c !== null && k !== null && (this.state.selectors[c] == null || this.state.selectors[c][k] == null)) { 
            axios.get(this.props.baseUri+'/classes/'+ c + '/selectors?category=' + k)
                .then(res => {
                    const current = this.state.selectors[c];
                    const novel = current == null ? {} : current;
                    novel[k] = res.data.sort();
                    this.setState({selectors: {[c]: novel}}) })
        }
    }

    getMethod = () => {
        const c = this.state.selectedClass;
        const s = this.state.selectedSelector;
        if (c == null || s == null) { return };
        axios.get(this.props.baseUri+'/classes/'+ c + '/methods/' + s)
            .then(res => { this.setState({selectedMethod: res.data}) })
    }

    currentCategories = () => {
        const c = this.state.selectedClass;
        return c == null ? [] : this.state.categories[c];
    }

    currentSelectors = () => {
        const c = this.state.selectedClass;
        const k = this.state.selectedCategory;
        return (c == null || k == null || this.state.selectors[c] == null) ? [] : this.state.selectors[c][k];
    }

    variables() {
        return this.state.selectedClass == null? [] : this.state.variables[this.state.selectedClass];
    }

    render() {
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={3} alignItems="stretch">
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={3}>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper}>
                                <ClassTree
                                    classes={this.state.classes}
                                    onSelect={this.classSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper}>
                                <SimpleList
                                    items={this.state.variables[this.state.selectedClass]}
                                    onSelect={this.variableSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper}>
                                <SimpleList
                                    items={this.currentCategories()}
                                    onSelect={this.categorySelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper}>
                                <SimpleList
                                    items={this.currentSelectors()}
                                    onSelect={this.selectorSelected}/>
                            </Paper>
                        </Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper>
                        <CodeEditor method={this.state.selectedMethod} />
                    </Paper>
                </Grid> 
            </Grid>
        )
    };
}

export default ClassBrowser
