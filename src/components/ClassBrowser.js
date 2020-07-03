import React, { Component } from "react";
import { Grid, Paper, Button, ButtonGroup } from "@material-ui/core";
import axios from 'axios';
import clsx from "clsx";

import SearchList from './SearchList';
import ClassTree from './ClassTree';
import SimpleList from './SimpleList';
import CodeEditor from './CodeEditor';

class ClassBrowser extends Component {
    constructor(props) {
        super(props);
        this.state = {
            root: this.props.root,
            classTree: [],
            classes: [],
            definitions: {},
            selectedClass: this.props.root,
            selectedVariable: null,
            selectedCategory: null,
            selectedSelector: null,
            variables: {},
            categories: {},
            selectors: {},
            selectedMethod: {selector: 'no method', source: 'nothing'},
            mode: "method"
        }
    }

    componentDidMount(){
        this.getClassTree();
        this.getClasses();
    }

    classSelected = (c) => {
        this.setState({selectedClass: c}, () => {
            this.getDefinition();
            this.getVariables();
            this.getCategories();
            //this.getSelectors()
        });
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
        axios.get(this.props.baseUri+'/classes?root='+this.state.root+'&tree=true')
            .then(res => {this.setState({classTree: res.data})}
        )
    }

    getClasses = () => {
        axios.get(this.props.baseUri+'/classes?names=true')
            .then(res => {this.setState({classes: res.data})}
        )
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
                .then(res => {
                    this.setState({
                        categories: {[c]: res.data.sort()},
                        selectedCategory: null}) 
                 })
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

    showMethodDefinition = () => {
        this.setState({mode: "method"})
    }

    showClassDefinition = () => {
        this.setState({mode: "class"})
    }
    
    source() {
        if (this.state.mode === "method") {
            return this.state.selectedMethod.source
         } else {
            return this.state.definitions[this.state.selectedClass];
         }
    }

    render() {
        const { classes, classTree, variables, selectedClass } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={3} alignItems="stretch">
                <Grid item xs={12} md={3} lg={3}>
                    <Paper>
                        <SearchList options={classes}/>
                    </Paper>
                </Grid> 
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={3}>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper}>
                                <ClassTree
                                    classes={classTree}
                                    onSelect={this.classSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper}>
                                <SimpleList
                                    items={variables[selectedClass]}
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
                    <ButtonGroup variant="text" aria-label="large outlined primary button group">
                        <Button onClick={this.showMethodDefinition}>Method defintion</Button>
                        <Button onClick={this.showClassDefinition}>Class definition</Button>
                    </ButtonGroup>
                </Grid>                 
                <Grid item xs={12} md={12} lg={12}>
                    <Paper>
                        <CodeEditor source={this.source()} />
                    </Paper>
                </Grid> 
            </Grid>
        )
    };
}

export default ClassBrowser
