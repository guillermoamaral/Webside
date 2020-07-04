import React, { Component } from 'react';
import { Grid, Paper, Button, ButtonGroup } from '@material-ui/core';
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
        this.state = {
            root: this.props.root,
            classTree: [],
            classes: [],
            definitions: {},
            comments: {},
            selectedClass: this.props.root,
            selectedVariable: null,
            selectedCategory: null,
            selectedSelector: null,
            variables: {},
            categories: {},
            selectors: {},
            selectedMethod: {selector: 'selector', source: '"no source"'},
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
            this.getCommment();
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
        axios.get(this.props.baseUri + '/classes?root='+this.state.root+'&tree=true')
            .then(res => {this.setState({classTree: res.data})}
        )
    }

    getClasses = () => {
        axios.get(this.props.baseUri + '/classes?names=true')
            .then(res => {this.setState({classes: res.data})}
        )
    }

    getDefinition = () => {
        const { selectedClass, definitions } = this.state;
        if (definitions[selectedClass] == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass)
                .then(res => {
                    definitions[selectedClass] = res.data;
                    this.setState({definitions: definitions})
                })
        }
    }

    getCommment = () => {
        const { selectedClass, comments } = this.state;
        if (comments[selectedClass] == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/comment')
                .then(res => {
                    const comment = (res.data == null || res.data == '') ? '"no comment"' : res.data;
                    comments[selectedClass] = comment;
                    this.setState({comments: comments})
                })
        }
    }

    getVariables = () => {
        const { selectedClass, variables } = this.state;
        if (variables[selectedClass] == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/instance-variables')
                .then(res => {
                    variables[selectedClass] = res.data.sort();
                    this.setState({variables: variables})
                })
        }
    }

    getCategories = () => {
        const { selectedClass, categories } = this.state;
        if (categories[selectedClass] == null) {
            axios.get(this.props.baseUri + '/classes/' +  selectedClass + '/categories')
                .then(res => {
                    const categories  = this.state.categories;
                    categories[selectedClass] = res.data.sort();
                    this.setState({categories: categories})
                 })
        }
    }

    getSelectors = () => {
        const { selectedClass, selectedCategory, selectors } = this.state;
        if (selectedClass !== null && selectedCategory !== null 
            && (selectors[selectedClass] == null || selectors[selectedClass][selectedCategory] == null)) { 
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/selectors?category=' + selectedCategory + '&marks=true')
                .then(res => {
                    if (selectors[selectedClass] == null) {
                        selectors[selectedClass] = {}
                    };
                    const sorted = res.data.sort(function(a, b){ return a.selector <= b.selector? -1 : 1 });
                    selectors[selectedClass][selectedCategory] = sorted;
                    this.setState({selectors: selectors}) 
                })
        }
    }

    getMethod = () => {
        const c = this.state.selectedClass;
        const s = this.state.selectedSelector;
        if (c == null || s == null) { return };
        axios.get(this.props.baseUri + '/classes/' + c + '/methods/' + s)
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

    showClassComment = () => {
        this.setState({mode: "comment"})
    }
    
    source() {
        var source;
        switch (this.state.mode) { 
            case "comment":
                source = this.state.comments[this.state.selectedClass];
                break;
            case "class":
                source = this.state.definitions[this.state.selectedClass];
                break;
            case "method":    
                source = this.state.selectedMethod.source;
                break;
            default:
                source = 'no source';
        };
        return source      
    }

    render() {
        const { classes, classTree, variables, selectedClass } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={3} alignItems="stretch">
                <Grid item xs={12} md={3} lg={3}>
                    <Paper>
                        {/*<SearchList options={classes}/>*/}
                    </Paper>
                </Grid> 
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={3}>
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
                                    items={variables[selectedClass]}
                                    onSelect={this.variableSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <SimpleList
                                    items={this.currentCategories()}
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
                    <ButtonGroup variant="text" aria-label="large outlined primary button group">
                        <Button onClick={this.showMethodDefinition} variant="outlined">Method defintion</Button>
                        <Button onClick={this.showClassDefinition} variant="outlined">Class definition</Button>
                        <Button onClick={this.showClassComment} variant="outlined">Class comment</Button>
                    </ButtonGroup>
                </Grid>                 
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeEditor source={this.source()} />
                    </Paper>
                </Grid> 
            </Grid>
        )
    };
}

export default ClassBrowser
