import React, { Component } from 'react';
import { Grid, Paper, RadioGroup, FormControlLabel, Radio } from '@material-ui/core';
import clsx from 'clsx';

import SearchList from './SearchList';
import ClassTree from './ClassTree';
import VariableList from './VariableList';
import CategoryList from './CategoryList';
import SelectorList from './SelectorList';
import CodeEditor from './CodeEditor';

class ClassBrowser extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        this.state = {
            root: this.props.root,
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

    getClassNames = () => {
        this.props.api.getClassNames()
            .then(names => {this.setState({classNames: names})})
            .catch(error => {})
    }

    changeRoot = (classname) => {
        this.setState({root: classname, classes: {}}, () => {
            this.updateClasses(classname)
                .then(() => {
                    this.classSelected(this.state.classes[classname])})
            //this.getClassNames();
        })
    }

    currentSelections() {
        return {
            class: this.state.selectedClass,
            variable: this.state.selectedVariable,
            category: this.state.selectedCategory,
            selector: this.state.selectedSelector,
            method: this.state.selectedMethod,
        };
    }

    applySelections(selections) {
        this.setState((prevState, props) => {
            const species = selections.class;
            const classes = prevState.classes;
            if (species !== undefined && classes[species.name] === undefined) {
                classes[species.name] = species;
            }
            return {
                classes: classes,
                selectedClass: selections.class,
                selectedVariable: selections.variable,
                selectedCategory: selections.category,
                selectedSelector: selections.selector,
                selectedMethod: selections.method,            
            }
        })
    }

    // Events...
    classSelected = async (species) => {
        const selections = this.currentSelections();
        selections.class = species;
        await this.updateDefinition(selections);
        await this.updateVariables(selections);
        await this.updateCategories(selections);
        await this.updateSelectors(selections);
        this.applySelections(selections)
    }

    // classExpanded = (species) => {
    //     species.subclasses.forEach(c => {
    //         this.updateClasses(species);
    //     })
    // }    

    variableSelected = async (variable) => {
        const selections = this.currentSelections();
        selections.variable = variable;
        await this.updateSelectors(selections);
        this.applySelections(selections);
    }

    categorySelected = async (category) => {
        const selections = this.currentSelections();
        selections.category = category;
        await this.updateSelectors(selections);
        this.applySelections(selections);
    }

    selectorSelected = async (selector) => {
        const selections = this.currentSelections();
        selections.selector = selector;
        await this.updateMethod(selections);
        this.applySelections(selections)   
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
            this.classSelected(current);
        } else {
            classes[species.name] = species;
            const superclass = classes[species.superclass];
            if (superclass !== undefined) {
                superclass.subclasses.push(species);
                superclass.subclasses.sort((a, b) => {return a.name <= b.name? -1 : 1});
            }
            this.setState({classes: classes}, () => this.classSelected(species))
        }
    }

    classCommented = (species) => {
        const classes = this.state.classes;
        classes[species.name].comment = species.comment;
        this.setState({classes: classes})
    }

    classRemoved = (species) => {
        const classes = this.state.classes;
        delete classes[species.name];
        const superclass = classes[species.superclass];
        if (superclass !== undefined) {
            superclass.subclasses = superclass.subclasses.filter(c => c !== species);
            this.setState({classes: classes}, () => this.classSelected(superclass));
        } else {
            this.changeRoot('Object')
        }
    }

    methodCompiled = async (method) => {
        const classes = this.state.classes;
        const species = classes[method.class];
        const selectors = species.selectors[method.category];
        var selector = selectors.find(s => {return s.selector === method.selector});
        const selections = this.currentSelections();
        if (selector === undefined) {
            await this.updateSelectors(selections, true);
            const selectors = species.selectors[method.category];
            selector = selectors.find(s => {return s.selector === method.selector});
        }
        this.selectorSelected(selector);
    }

    selectorRemoved = (selector) => {
        const classes = this.state.classes;
        const category = this.state.selectedCategory;
        var selectors = classes[selector.class].selectors[category];
        classes[selector.class].selectors[category] = selectors.filter(s => {return s.selector !== selector.selector});
        this.setState({classes: classes})
    }

    // Updating...
    async updateClasses(root) {
        const classes = this.state.classes;
        const tree = await this.props.api.getClassTree(root, 2);
        tree.forEach(c => {classes[c.name] = c});
    }

    async updateDefinition(selections, force = false) {
        const species = selections.class;
        if (force || species.definitionString === undefined) {
            const definition = await this.props.api.getDefinition(species.name);
            species.definitionString = definition.definitionString;
            species.comment = definition.comment;
            species.superclass = definition.superclass;
        }
    }
    
    async updateVariables(selections, force = false) {
        const species = selections.class;
        if (force || species.variables === undefined) {
            species.variables = await this.props.api.getVariables(species.name);
        }
        var variable = selections.variable;
        if (variable !== null) {
            variable = species.variables.find(v => {return v.name === variable.name});
            selections.variable = variable === undefined ? null : variable;
        }
    }

    async updateCategories(selections, force = false) {
        const species = selections.class; 
        if (force || species.categories === undefined) {
            const categories = await this.props.api.getCategories(species.name);
            species.categories = categories.sort();
        }
        if (!species.categories.includes(selections.category)) {
            selections.category = null;
        }
    }

    async updateSelectors(selections, force = false) {
        const species = selections.class;
        const category = selections.category;
        if (species.selectors === undefined) {species.selectors = {}};
        if ((force || species.selectors[category] === undefined) && category !== null) {
            const selectors = await this.props.api.getSelectors(species.name, category);
            species.selectors[category] = selectors.sort((a, b) => {return a.selector <= b.selector? -1 : 1});
        }
        var selector = selections.selector;
        if (selector !== null && category !== null) {
            selector = species.selectors[category].find(s => {return s.selector === selector.selector});
            selections.celector = selector === undefined ? null : selector;    
        }
    }

    async updateMethod(selections, force = true) {
        const species = selections.class;
        const selector = selections.selector;
        if (force) {
            const method = await this.props.api.getMethod(species.name, selector.selector);
            selections.method = method;
        }
    }

    // Contents..
    currentVariables() {
        const species = this.state.selectedClass;
        return (species == null || species.variables == null)? [] : species.variables;
    }

    currentCategories = () => {
        const species = this.state.selectedClass;
        return (species == null || species.categories == null)? [] : species.categories;
    }

    currentSelectors = () => {
        const species = this.state.selectedClass;
        const category = this.state.selectedCategory;
        return (species == null || category == null
            || species.selectors == null)? [] : species.selectors[category];
    }

    render() {
        const {
            root,
            classes,
            selectedClass,
            selectedVariable,
            selectedCategory,
            selectedSelector,
            selectedMethod} = this.state;
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
                                <ClassTree
                                    api={this.props.api}
                                    globalOptions={this.props.globalOptions}
                                    classes={classes}
                                    root={root}
                                    selectedClass={selectedClass}
                                    onExpand={this.classExpanded}
                                    onSelect={this.classSelected}
                                    onRemoved={this.classRemoved}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper  className={fixedHeightPaper} variant="outlined">
                                <VariableList
                                    variables={this.currentVariables()}
                                    selectedVariable={selectedVariable}
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
                                        <CategoryList
                                            categories={this.currentCategories()}
                                            selectedCategory={selectedCategory}
                                            onSelect={this.categorySelected}/>
                                    </Paper>
                                </Grid>
                            </Grid>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            <Paper  className={fixedHeightPaper} variant="outlined">
                                <SelectorList
                                    api={this.props.api}
                                    globalOptions={this.props.globalOptions}
                                    selectors={this.currentSelectors()}
                                    selectedSelector={selectedSelector}
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
