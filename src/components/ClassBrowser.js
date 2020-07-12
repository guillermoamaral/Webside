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
            this.updateClassDefinition(species);
            this.updateVariables(species);
            this.updateCategories(species)
        })
    }

    variableSelected = (variable) => {
        this.setState({selectedVariable: variable});
    }

    categorySelected = (category) => {
        this.setState({selectedCategory: category}, () => {
            this.updateSelectors(this.state.selectedClass, category)
        });
    }

    selectorSelected = (selector) => {
        this.setState({selectedSelector: selector}, () => {
            this.updateMethod(this.state.selectedClass, selector)
        });
    }

    getClassTree = () => {
        this.props.api.classTree(this.state.root)
            .then(tree => {
                this.setState({classTree: tree})})
            .catch(error => {})
    }

    getClassNames = () => {
        this.props.api.classNames()
            .then(names => {this.setState({classNames: names})})
            .catch(error => {})
    }

    updateClassDefinition = (species) => {
        const { classes } = this.state;
        if (species.definitionString == null) {
            this.props.api.definitionOf(species.name)
                .then(definition => {
                    species.definitionString = definition.definitionString;
                    species.comment = definition.comment;
                    species.superclass = definition.superclass;
                    this.setState({classes: classes})})
                .catch(error => {console.log('good fart')})
        }
    }

    updateVariables = (species) => {
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

    updateCategories = (species) => {
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

    updateSelectors = (species, category, force = false) => {
        const { classes } = this.state;
        if (force || species.selectors == null || species.selectors[category] == null) {
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

    updateMethod = (species, selector) => {
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

    sideChanged = (event, side) => {
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
        const current = classes[species.name];
        if (current !== undefined) {
            current.definitionString = species.definitionString;
            this.setState({classes: classes}, () => {this.updateVariables(classes[species.name])})
        } else {
            this.changeRoot(species.name)
        }
    }

    classCommented = (species) => {
        const classes = this.state.classes;
        classes[species.name].comment = species.comment;
        this.setState({classes: classes})
    }

    methodCompiled = (method) => {
        const classes = this.state.classes;
        const selectors = classes[method.class].selectors[method.category];
        var selector = selectors.find(s => { return s.selector === method.selector });
        if (selector === undefined) {
            this.updateSelectors(classes[method.class], method.category, true);
            selector = selectors.find(s => s.selector === method.selector);
        } 
        this.setState({classes: classes}, () => {this.selectorSelected(selector)})
    }

    removeClass = (selector) => {
        console.log(selector)
    }

    selectorRemoved = (selector) => {
        const classes = this.state.classes;
        const category = this.state.selectedCategory;
        var selectors = classes[selector.class].selectors[category];
        classes[selector.class].selectors[category] = selectors.filter(s => { return s.selector !== selector.selector});
        this.setState({classes: classes})
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
                                    onSelect={this.classSelected}
                                    menuOptions={[{label: 'Remove', action: this.removeClass}]}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper  className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    items={this.currentVariables()}
                                    label="name"
                                    selectedItem={selectedVariable}
                                    onSelect={this.variableSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Grid container spacing={1} justify="center">
                                <Grid item xs={12} md={12} lg={12}>
                                    <RadioGroup
                                        name="side"
                                        value={this.state.side}
                                        onChange={this.sideChanged}
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
                                    api={this.props.api}
                                    selectors={this.currentSelectors()}
                                    onSelect={this.selectorSelected}
                                    onRemoved={this.selectorRemoved}
                                    />
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
