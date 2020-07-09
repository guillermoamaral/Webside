import React, { Component } from 'react';
import { Grid, Paper, RadioGroup, FormControlLabel, Radio } from '@material-ui/core';
import clsx from 'clsx';

import SearchList from './SearchList';
import CustomTree from './CustomTree';
import SelectorList from './SelectorList';
import CustomList from './CustomList';
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
        this.changeRoot(this.state.root);
    }

    changeRoot = (root) => {
        const selected = this.state.selectedClass !== null;
        this.setState({root: root}, () => {
            this.getClassTree();
            this.getClassNames();
            if (selected) { this.classSelected(root) };
        })
    }

    classSelected = (name) => {
        const classes = this.state.classes; 
        if (classes[name] == null) { classes[name] = {name: name} };
        const species = classes[name];
        this.setState({classes: classes, selectedClass: species}, () => {
            this.getClassDefinition(species);
            this.getVariables(species);
            this.getCategories(species)
        })
    }

    variableSelected = (variable) => {
        this.setState({selectedVariable: variable});
    }

    categorySelected = (category) => {
        this.setState({selectedCategory: category}, () => {
            this.getSelectors(this.state.selectedClass, category)
        });
    }

    selectorSelected = (selector) => {
        this.setState({selectedSelector: selector}, () => {
            this.getMethod(this.state.selectedClass, selector)
        });
    }

    getClassTree = () => {
        this.props.api.classTree(this.state.root)
            .then(tree => {this.setState({classTree: tree})})
            .catch(error => {})
    }

    getClassNames = () => {
        this.props.api.classNames()
            .then(names => {this.setState({classNames: names})})
            .catch(error => {})
    }

    getClassDefinition = (species) => {
        const { classes } = this.state;
        if (species.definitionString == null) {
            this.props.api.definitionOf(species.name)
                .then(definition => {
                    species.definitionString = definition.definitionString;
                    species.comment = definition.comment;
                    species.superclass = definition.superclass;
                    this.setState({classes: classes})})
                .catch(error => {})
        }
    }

    getVariables = (species) => {
        const { classes, selectedVariable } = this.state;
        if (species.variables == null) {
            this.props.api.variablesOf(species.name)
                .then(variables => {
                    species.variables = variables;
                    var selected = selectedVariable;
                    if (selected !== null && (variables.find(v => v.name === selected.name)) === undefined) {
                        selected = null;
                    }
                    this.setState({classes: classes, selectedVariable: selected})})
                .catch(error => {})
        }
    }

    getCategories = (species) => {
        const { classes, selectedCategory } = this.state;
        if (species.categories == null) {
            this.props.api.categoriesOf(species.name)
                .then(categories => {
                    species.categories = categories.sort();
                    var selected = selectedCategory;
                    if (!categories.includes(selected)) {
                        selected = null;
                    }
                    this.setState({classes: classes, selectedCategory: selected})})
                .catch(error => {})
        }
    }

    getSelectors = (species, category) => {
        const { classes } = this.state;
        if (species.selectors == null || species.selectors[category] == null) { 
            this.props.api.selectorsOf(species.name, category)
                .then(selectors => {
                    if (species.selectors == null) {
                        species.selectors = {}
                    };
                    const sorted = selectors.sort((a, b) => { return a.selector <= b.selector? -1 : 1 });
                    species.selectors[category] = sorted;
                    this.setState({classes: classes})})
                .catch(error => {})
        }
    }

    getMethod = (species, selector) => {
        //Should not happen that..
        //if (species == null || selector == null) { return };
        this.props.api.method(species.name, selector.selector)
            .then(method => {this.setState({selectedMethod: method})})
            .catch(error => {})
    }

    currentVariables() {
        const { selectedClass } = this.state;
        if (selectedClass == null || selectedClass.variables == null) { return [] };
        return selectedClass.variables;
    }

    currentCategories = () => {
        const { selectedClass } = this.state;
        if (selectedClass == null || selectedClass.categories == null) { return [] };
        return selectedClass.categories;
    }

    currentSelectors = () => {
        const { selectedClass, selectedCategory } = this.state;
        if (selectedClass == null || selectedCategory == null || selectedClass.selectors == null) { return [] };
        return selectedClass.selectors[selectedCategory];
    }

    changeSide = (event, side) => {
        if (side == null) return;
        this.setState({side: side});
        if (side === "instance") {
            const name = this.state.root;
            this.changeRoot(name.slice(0, name.length - 6))
        } else {
            this.changeRoot(this.state.root + " class")
        }
    }

    classDefined = (species) => {
        const classes = this.state.classes;
        classes[species.name].definitionString = species.definitionString;
        this.setState({classes: classes}, () => {this.getVariables(classes[species.name])})
    }

    classCommented = (species) => {
        const classes = this.state.classes;
        classes[species.name].comment = species.comment;
        this.setState({classes: classes})
    }

    methodCompiled = (method) => {
        const classes = this.state.classes;
        const selectors = classes[method.class].selectors[method.category];
        if (selectors.find(s => s.selector === method.selector) === undefined) {
            selectors.push({selector: method.selector});
            selectors.sort((a, b) => { return a.selector <= b.selector? -1 : 1 })
        }
        this.setState({classes: classes}, () => {this.selectorSelected(method.selector)})
    }

    render() {
        const {
            classTree,
            classes,
            selectedClass,
            selectedVariable,
            selectedCategory,
            selectedSelector,
            selectedMethod } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        const fixedHeightPaper2 = clsx(this.props.classes.paper, this.props.classes.fixedHeight2);
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
                                <CustomTree
                                    items={classTree}
                                    label="name"
                                    children={"subclasses"}
                                    onSelect={this.classSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper  className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    items={this.currentVariables()}
                                    label="name"
                                    onSelect={this.variableSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Grid container spacing={1} justify="center">
                                <Grid item xs={12} md={12} lg={12}>
                                    <RadioGroup
                                        name="side"
                                        value={this.state.side}
                                        onChange={this.changeSide}
                                        defaultValue="instance"
                                        className={this.props.classes.radioGroup}
                                        size="small"
                                        row
                                        >
                                        <FormControlLabel className={this.props.radioButton} value="instance" control={<Radio size="small" color="primary"/>} label="Instance"/>
                                        <FormControlLabel className={this.props.radioButton} value="class" control={<Radio size="small" color="primary"/>} label="Class" />
                                    </RadioGroup>
                                </Grid>
                                <Grid item xs={12} md={12} lg={12}>
                                    <Paper className={fixedHeightPaper2} variant="outlined">
                                        <CustomList
                                            items={this.currentCategories()}
                                            selectedItem={selectedCategory}
                                            onSelect={this.categorySelected}/>
                                    </Paper>
                                </Grid>
                            </Grid>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper  className={fixedHeightPaper} variant="outlined">
                                <SelectorList
                                    selectors={this.currentSelectors()}
                                    onSelect={this.selectorSelected}/>
                            </Paper>
                        </Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined" height="100%">
                        <CodeEditor
                            classes={this.props.classes}
                            api={this.props.api}
                            class={selectedClass == null ? '' : selectedClass.name}
                            definition={selectedClass == null ? '' : selectedClass.definitionString}
                            comment={selectedClass == null ? '' : selectedClass.comment}
                            category={selectedCategory}
                            selector={selectedSelector == null ? '' : selectedSelector.selector}
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

export default ClassBrowser;
