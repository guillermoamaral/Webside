import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import ToggleButton from "@material-ui/lab/ToggleButton";
import ToggleButtonGroup from "@material-ui/lab/ToggleButtonGroup";
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
            mode: "method",
            side: "instance"
        }
    }

    componentDidMount(){
        this.getClassTree();
        this.getClasses();
    }

    changeRoot = (root) => {
        this.setState({root: root}, () => {
            this.getClassTree();
            this.getClasses();
            this.classSelected(root);
        })
    }

    classSelected = (c) => {
        this.setState({selectedClass: c}, () => {
            this.getDefinition();
            this.getVariables();
            this.getCategories();
            this.getCommment();
            //this.getSelectors()
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
                    const comment = (res.data == null || res.data === '') ? '"no comment"' : res.data;
                    comments[selectedClass] = comment;
                    this.setState({comments: comments})
                })
        }
    }

    getVariables = () => {
        var { selectedClass, variables, selectedVariable } = this.state;
        if (variables[selectedClass] == null) {
            axios.get(this.props.baseUri + '/classes/' + selectedClass + '/instance-variables')
                .then(res => {
                    variables[selectedClass] = res.data;
                    if (!variables[selectedClass].includes(selectedVariable)) {
                        selectedVariable = null;
                    }
                    this.setState({variables: variables, selectedVariable: selectedVariable})
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

    changeMode = (e, mode) => {
        this.setState({mode: mode})
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
            <Grid container spacing={1} >
                <Grid item xs={12} md={6} lg={6}>
                    <Paper>
                        {/*<SearchList options={classes}/>*/}
                    </Paper>
                </Grid>
                <Grid item xs={12} md={3} lg={3}>
                    <ToggleButtonGroup
                        value={this.state.side}
                        exclusive
                        onChange={this.changeSide}>
                        <ToggleButton value="instance" variant="outlined" size="small">
                            Instance
                        </ToggleButton>
                        <ToggleButton value="class" variant="outlined" size="small">
                            Class
                        </ToggleButton>
                    </ToggleButtonGroup>
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
                                    items={variables[selectedClass]}
                                    selectedItem={this.selectedVariable}
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
                    <ToggleButtonGroup
                        value={this.state.mode}
                        exclusive
                        onChange={this.changeMode}>
                        <ToggleButton value="method" variant="outlined" size="small">
                            Method defintion
                        </ToggleButton>
                        <ToggleButton value="class" variant="outlined" size="small">
                            Class definition
                        </ToggleButton>
                        <ToggleButton value="comment" variant="outlined" size="small">
                            Class comment
                        </ToggleButton>
                    </ToggleButtonGroup>
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
