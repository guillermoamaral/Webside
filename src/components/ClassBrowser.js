import React, { Component } from 'react';
import { Grid, Paper, RadioGroup, FormControlLabel, Radio } from '@material-ui/core';
import clsx from 'clsx';

import SearchList from './SearchList';
import ClassTree from './ClassTree';
import SelectorList from './SelectorList';
import CustomList from './CustomList';
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
        this.props.api.classNames()
            .then(names => {this.setState({classNames: names})})
            .catch(error => {})
    }

    changeRoot = (classname) => {
        this.setState({root: classname}, () => {
            this.updateClasses();
            //this.getClassNames();
        })
    }

    selections() {
        return {
            selectedClass: this.state.selectedClass,
            selectedVariable: this.state.selectedVariable,
            selectedCategory: this.state.selectedCategory,
            selectedSelector: this.state.selectedSelector,
            selectedMethod: this.state.selectedMethod,
        };
    }

    reviseSelections(selections) {
        const species = selections.selectedClass;
        if (species !== null) {
            var variable = selections.selectedVariable;
            if (variable !== null) {
                variable = species.variables.find(v => { return v.name === variable.name });
                selections.seletectedVariable = variable === undefined ? null : variable;
            }
            if (!species.categories.includes(selections.selectedCategory)) {
                selections.selectedCategory = null;
            }
            const category = selections.selectedCategory;
            var selector = selections.selectedSelector;
            if (selector !== null && category !== null) {
                selector = species.selectors[category].find(s => { return s.selector === selector.selector });
                selections.seletectedSelector = selector === undefined ? null : selector;    
            }
        }
    }

    applySelections(selections) {
        console.log('applying selections')
        console.log(selections)
        this.setState((prevState, props) => {
            return {
                selectedClass: selections.selectedClass,
                selectedVariable: selections.selectedVariable,
                selectedCategory: selections.selectedCategory,
                selectedSelector: selections.selectedSelector,
                selectedMethod: selections.selectedMethod,            
            }
        })
    }

    updateClasses = () => {
        this.props.api.classTree(this.state.root)
            .then(classes => {
                const map = {};
                classes.forEach(c => { map[c.name] = c });
                var selected = this.state.selectedClass;
                if (selected !== null) {
                    selected = classes.find(c => { return c.name = selected.name });
                    if (selected === undefined) { selected = null }
                }
                this.setState({classes: map, selectedClass: selected})})
            .catch(error => {})
    }

    classSelected = async (species) => {
        const selections = this.selections();
        selections.selectedClass = species;
        try {
            console.log('updating definition')
            await this.updateDefinition(selections);
            console.log('updating variables')
            await this.updateVariables(selections);
            console.log('updating categories')
            await this.updateCategories(selections);
            console.log('updating selectors')
            await this.updateSelectors(selections);
            console.log('applying selections')
            this.applySelections(selections)   
        }
        catch (error) {
            this.reportError(error)
        }
    }

    // classExpanded = (species) => {
    //     species.subclasses.forEach(c => {
    //         this.updateDefinition(c);
    //         this.updateSubclasses(c);
    //     })
    // }    

    variableSelected = async (variable) => {
        const selections = this.selections();
        selections.selectedVariable = variable;
        try {
            await this.updateSelectors(selections);
            this.applySelections(selections)   
        }
        catch (error) {
            this.reportError(error)
        }
    }

    categorySelected = async (category) => {
        const selections = this.selections();
        selections.selectedCategory = category;
        try {
            await this.updateSelectors(selections);
            this.applySelections(selections)   
        }
        catch (error) {
            this.reportError(error)
        }
    }

    selectorSelected = async (selector) => {
        const selections = this.selections();
        selections.selectedSelector = selector;
        try {
            console.log('updating method')
            await this.updateMethod(selections);
            this.applySelections(selections)   
        }
        catch (error) {
            this.reportError(error)
        }
    }

    updateDefinition(selections, force = false) {
        const species = selections.selectedClass;
        if (force || species.definitionString == null) {
            return this.props.api.definitionOf(species.name)
                .then(definition => {
                    species.definitionString = definition.definitionString;
                    species.comment = definition.comment;
                    species.superclass = definition.superclass;
                })
        }
    }
    
    updateVariables(selections, force = false) {
        const species = selections.selectedClass;
        if (force || species.variables == null) {
            return this.props.api.variablesOf(species.name)
                .then(variables => {species.variables = variables; console.log('variables here')})
        }
    }

    updateCategories(selections, force = false) { 
        const species = selections.selectedClass;
        if (force || species.categories == null) {
            return this.props.api.categoriesOf(species.name)
                .then(categories => {species.categories = categories.sort(); console.log('categories here')});
        }
    }

    updateSelectors(selections, force = false) {
        this.reviseSelections(selections);
        const species = selections.selectedClass;
        if (species.selectors == null) { species.selectors = {} };
        const category = selections.selectedCategory;
        if ((force || species.selectors[category] == null) && category !== null) {
            return this.props.api.selectorsOf(species.name, category)
                .then(selectors => {
                    const sorted = selectors.sort((a, b) => { return a.selector <= b.selector? -1 : 1 });
                    species.selectors[category] = sorted;     
                    console.log('selectors here')   
                })
        }
    }

    updateMethod(selections) {
        const species = selections.selectedClass;
        const selector = selections.selectedSelector;
        return this.props.api.method(species.name, selector.selector)
            .then(method => {selections.selectedMethod = method; console.log('method here')})
    }

    currentVariables() {
        const { selectedClass } = this.state;
        if (selectedClass == null || selectedClass.variables == null) {
            return []
        } else {
            return selectedClass.variables;
        }
    }

    currentCategories = () => {
        const { selectedClass } = this.state;
        if (selectedClass == null || selectedClass.categories == null) {
            return []
        } else {
            return selectedClass.categories;
        }
    }

    currentSelectors = () => {
        const { selectedClass, selectedCategory } = this.state;
        if (selectedClass == null || selectedCategory == null || selectedClass.selectors == null) {
            return []
        } else {
            return selectedClass.selectors[selectedCategory];
        }
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

    // Changes...
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

    classRemoved = (species) => {
        console.log(species)
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
        var updates = false;
        if (selector === undefined) {
            selector = {class: method.class, selector: method.selector};
            updates = true;
        } 
        this.setState({classes: classes, selectedSelector: selector, selectedMethod: method});
        if (updates) { this.updateSelectors(classes[method.class], method.category, true) }
    }

    selectorRemoved = (selector) => {
        const classes = this.state.classes;
        const category = this.state.selectedCategory;
        var selectors = classes[selector.class].selectors[category];
        classes[selector.class].selectors[category] = selectors.filter(s => { return s.selector !== selector.selector });
        this.setState({classes: classes})
    }

    render() {
        const {
            root,
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
