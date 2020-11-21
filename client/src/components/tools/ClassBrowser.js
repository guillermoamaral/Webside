import React, { Component } from 'react';
import {
    Grid,
    Box,
    Paper,
    RadioGroup,
    FormControlLabel,
    Radio,
    Select,
    MenuItem,
    OutlinedInput
} from '@material-ui/core';
import clsx from 'clsx';
import { IDEContext } from '../IDEContext';
import SearchList2 from '../controls/SearchList2';
import ClassTree from '../parts/ClassTree';
import VariableList from '../parts/VariableList';
import CategoryList from '../parts/CategoryList';
import MethodList from '../parts/MethodList';
import CodeBrowser from '../parts/CodeBrowser';

class ClassBrowser extends Component {
    static contextType = IDEContext;
    constructor(props) {
        super(props);
        this.cache = {};
        this.state = {
            root: this.props.root,
            selectedClass: null,
            selectedVariableAccess: "referencing",
            selectedVariable: null,
            selectedCategory: null,
            selectedMethod: null,
            selectedSide: "instance"
        }
    }

    componentDidMount(){
        const root = this.state.root;
        if (root) {this.changeRootClass(root)}
    }

    changeRootClass = async (name) => {
        if (!name) {return}
        const tree = await this.context.api.getClassTree(name, 3);
        const species = tree[0];
        this.cache[name] = species;
        this.setState({root: name}, () => {this.classSelected(species)});
    }

    currentSelections() {
        return {
            class: this.state.selectedClass,
            variableAccess: this.state.selectedVariableAccess,
            variable: this.state.selectedVariable,
            category: this.state.selectedCategory,
            method: this.state.selectedMethod,
        };
    }

    applySelections(selections) {
        this.setState((prevState, props) => {
            const species = selections.class;
            if (species && !this.cache[species.name]) {
                this.cache[species.name] = species;
            }
            return {
                selectedClass: selections.class,
                selectedVariableAccess: selections.variableAccess,
                selectedVariable: selections.variable,
                selectedCategory: selections.category,
                selectedMethod: selections.method,
            }
        })
    }

    // Contents..
    currentVariables() {
        const species = this.state.selectedClass;
        return (!species || !species.variables)? [] : species.variables;
    }

    currentCategories = () => {
        const species = this.state.selectedClass;
        return (!species || !species.categories)? [] : species.categories;
    }

    currentMethods = () => {
        const species = this.state.selectedClass;
        const category = this.state.selectedCategory;
        const variable = this.state.selectedVariable;
        const access = this.state.selectedVariableAccess;
        if (!species) {return []}
        var methods = species.methods;
        if (category) {methods = methods.filter(m => m.category === category)}
        if (variable) {
            const accessors = species[variable.name][access];
            methods = methods.filter(m => accessors.some(n => n.selector === m.selector))
        }
        return methods;
    }

    // Updating...
    async updateClass(selections, force = false) {
        const species = selections.class;
        if (force || !species.definition) {
            const definition = await this.context.api.getClass(species.name);
            species.definition = definition.definition;
            species.comment = definition.comment;
            species.superclass = definition.superclass;
        }
        if (force || !species.subclasses) {
            species.subclasses = await this.context.api.getSubclasses(species.name);
        }
    }

    async updateSubclasses(species) { 
        if (species.subclasses) {
            await Promise.all(species.subclasses.map(async c => {
                if (!c.subclasses) {
                    c.subclasses = await this.context.api.getSubclasses(c.name);
                }
            }))
        }
    }
    
    async updateVariables(selections, force = false) {
        const species = selections.class;
        if (force || !species.variables) {
            species.variables = await this.context.api.getVariables(species.name);
        }
        var variable = selections.variable;
        if (variable) {
            variable = species.variables.find(v => v.name === variable.name);
            selections.variable = !variable? null : variable;
        }
    }

    async updateCategories(selections, force = false) {
        const species = selections.class; 
        if (force || !species.categories) {
            const categories = await this.context.api.getCategories(species.name);
            species.categories = categories.sort();
        }
        if (!species.categories.includes(selections.category)) {
            selections.category = null;
        }
    }

    async updateMethods(selections, force = false) {
        const species = selections.class;
        if (!species) {return}
        if (force || !species.methods) {
            const methods = await this.context.api.getMethods(species.name);
            species.methods = methods.sort((a, b) => a.selector <= b.selector? -1 : 1);
        }
        const variable = selections.variable;
        const variableAccess = selections.variableAccess;
        if (variable && (force || !species[variable.name] || !species[variable.name][variableAccess])) {
            const accessors = await this.context.api.getMethodsAccessing(species.name, variable.name, variableAccess);
            species[variable.name] = {};
            species[variable.name][variableAccess] = accessors.sort((a, b) => a.selector <= b.selector? -1 : 1);
        }
        var method = selections.method;
        if (method) {
            method = species.methods.find(m => m.selector === method.selector);
            selections.method = !method? null : method;    
        }
    }

    async updateMethod(selections, force = true) {
        const species = selections.class;
        const selector = selections.method.selector;    
        if (force) {
            const method = await this.context.api.getMethod(species.name, selector);
            if (method) { 
                species.methods = species.methods.map(m => m.selector === selector? method : m)
                selections.method = method;
            }
        }
    }

    // Events...
    sideChanged = (event, side) => {
        if (!side) return;
        this.setState({selectedSide: side});
        if (!this.state.root) {return}
        if (side === "instance") {
            const name = this.state.root;
            this.changeRootClass(name.slice(0, name.length - 6))
        } else {
            this.changeRootClass(this.state.root + " class")
        }
    }

    classSelected = async (species) => {
        const selections = this.currentSelections();
        selections.class = species;
        await this.updateClass(selections);
        await this.updateSubclasses(species);
        await this.updateVariables(selections);
        await this.updateCategories(selections);
        await this.updateMethods(selections);
        this.applySelections(selections)
    }

    classExpanded = async (species) => {
        await this.updateSubclasses(species)
    }

    classDefined = async (species) => {
        var cached = this.cache[species.name];
        if (cached) {
            cached.definition = species.definition;
        } else {
            this.cache[species.name] = species;
            cached = species;
            const superclass = this.cache[species.superclass];
            if (superclass) {
                superclass.subclasses.push(species);
                superclass.subclasses.sort((a, b) => a.name <= b.name? -1 : 1);
            }
        }
        const selections = this.currentSelections();
        selections.class = cached;
        await this.updateVariables(selections, true);
        this.classSelected(cached);
    }
    
    classCommented = async (species) => {
        this.cache[species.name].comment = species.comment;
    }
    
    classRemoved = (species) => {
        delete this.cache[species.name];
        const superclass = this.cache[species.superclass];
        if (superclass) {
            superclass.subclasses = superclass.subclasses.filter(c => c !== species);
            this.classSelected(superclass);
        } else {
            this.changeRootClass('Object')
        }
    }

    classRenamed = (species) => {
        this.classSelected(species)
    }

    variableAccessSelected = async (event) => {
        const access = event.target.value;
        const selections = this.currentSelections();
        selections.variableAccess = access;
        await this.updateMethods(selections);
        this.applySelections(selections);
    } 

    variableSelected = async (variable) => {
        const selections = this.currentSelections();
        selections.variable = variable;
        await this.updateMethods(selections);
        this.applySelections(selections);
    }

    variableAdded = async (variable) => {        
        const selections = this.currentSelections();
        await this.updateVariables(selections, true);
        await this.updateMethods(selections, true);
        this.variableSelected(selections.class.variables.find(v => v.name === variable.name));
    }

    variableRenamed = async (variable) => {
        const selections = this.currentSelections();
        await this.updateVariables(selections, true);
        await this.updateMethods(selections, true);
        this.variableSelected(selections.class.variables.find(v => v.name === variable.name));
    }

    variableRemoved = async (variable) => {        
        const selections = this.currentSelections();
        await this.updateVariables(selections, true);
        await this.updateMethods(selections);
        this.applySelections(selections);
    }

    categorySelected = async (category) => {
        const selections = this.currentSelections();
        selections.category = category;
        await this.updateMethods(selections);
        this.applySelections(selections);
    }

    categoryAdded = async (category) => {        
        const selections = this.currentSelections();
        selections.category = category;
        selections.class.categories.push(category);
        selections.class.categories.sort();
        this.applySelections(selections);
    }

    categoryRenamed = async (category, renamed) => {
        const selections = this.currentSelections();
        await this.updateCategories(selections, true);
        await this.updateMethods(selections, true);
        this.categorySelected(selections.class.categories.find(c => c === renamed));
    }

    categoryRemoved = async (category) => {        
        const selections = this.currentSelections();
        selections.category = null;
        await this.updateCategories(selections, true);
        await this.updateMethods(selections);
        this.applySelections(selections);
    }    

    methodSelected = async (method) => {
        const selections = this.currentSelections();
        selections.method = method;
        await this.updateMethod(selections);
        this.applySelections(selections);
    }

    methodRenamed = (method) => {
        this.methodSelected(method);
    }

    methodRemoved = (method) => {
        this.cache[method.class].methods = this.cache[method.class].methods.filter(m => m.selector !== method.selector);
        this.setState({selectedMethod: null})
    }

    methodCompiled = async (method) => {
        const selections = this.currentSelections();
        const species = this.cache[method.class];
        selections.class = species;
        if (!species.categories.includes(method.category)) {
            await this.updateCategories(selections, true);
        }
        selections.category = species.categories.find(c => c === method.category);
        const methods = species.methods;
        const index = methods? methods.findIndex(m => m.selector === method.selector) : -1;
        if (index === -1) {
            await this.updateMethods(selections, true);
            selections.method = species.methods.find(m => m.selector === method.selector);
        } else {
            methods.splice(index, 1, method);
            selections.method = method;
        }
        this.applySelections(selections);
    }

    render() {
        const {
            root,
            selectedSide,
            selectedClass,
            selectedVariableAccess,
            selectedVariable,
            selectedCategory,
            selectedMethod} = this.state;
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={0}>
                        <Grid item xs={11} md={11} lg={11}>
                            <Grid container spacing={1}>
                                <Grid item xs={3} md={3} lg={3}>
                                    <SearchList2
                                        options={this.context.classNames}
                                        onChange={classname => {this.changeRootClass(classname)}}/>
                                </Grid>                        
                                <Grid item xs={3} md={3} lg={3}>
                                    <Select
                                        value={selectedVariableAccess}
                                        input={<OutlinedInput margin='dense' fullWidth/>}
                                        onChange={this.variableAccessSelected}>
                                            <MenuItem value={"using"}>using</MenuItem>
                                            <MenuItem value={"assigning"}>assigning</MenuItem>
                                            <MenuItem value={"referencing"}>referencing</MenuItem>
                                            <MenuItem value={"unusing"}>unusing</MenuItem>
                                    </Select>
                                </Grid>
                                <Grid item xs={3} md={3} lg={3}>
                                    <Box display="flex" justifyContent="center">
                                        <RadioGroup
                                            name="side"
                                            value={selectedSide}
                                            onChange={this.sideChanged}
                                            defaultValue="instance"
                                            row>
                                            <FormControlLabel value="instance" control={<Radio size="small" color="primary"/>} label="Instance"/>
                                            <FormControlLabel value="class" control={<Radio size="small" color="primary"/>} label="Class" />
                                        </RadioGroup>
                                    </Box>
                                </Grid>
                                <Grid item xs={3} md={3} lg={3}/>
                                <Grid item xs={12} md={3} lg={3}>
                                    <Paper className={fixedHeightPaper} variant="outlined">
                                        <ClassTree
                                            roots={root? this.cache[root]? [this.cache[root]] : [] : []}
                                            selectedClass={selectedClass}
                                            onExpand={this.classExpanded}
                                            onSelect={this.classSelected}
                                            onRemove={this.classRemoved}
                                            onRename={this.classRenamed}
                                            onCreate={this.classDefined}/>
                                    </Paper>
                                </Grid>
                                <Grid item xs={12} md={3} lg={3}>
                                    <Paper className={fixedHeightPaper} variant="outlined">
                                        <VariableList
                                            class={selectedClass}
                                            variables={this.currentVariables()}
                                            selectedVariable={selectedVariable}
                                            onAdd={this.variableAdded}
                                            onRename={this.variableRenamed}
                                            onSelect={this.variableSelected}
                                            onRemove={this.variableRemoved}/>
                                    </Paper>
                                </Grid>
                                <Grid item xs={12} md={3} lg={3}>
                                    <Paper className={fixedHeightPaper} variant="outlined">
                                        <CategoryList
                                            class={selectedClass}
                                            categories={this.currentCategories()}
                                            selectedCategory={selectedCategory}
                                            onAdd={this.categoryAdded}
                                            onRename={this.categoryRenamed}
                                            onSelect={this.categorySelected}
                                            onRemove={this.categoryRemoved}/>
                                    </Paper>
                                </Grid>
                                <Grid item xs={12} md={3} lg={3}>
                                    <Paper className={fixedHeightPaper} variant="outlined">
                                        <MethodList
                                            methods={this.currentMethods()}
                                            selectedMethod={selectedMethod}
                                            onSelect={this.methodSelected}
                                            onRename={this.methodRenamed}
                                            onRemove={this.methodRemoved}/>
                                    </Paper>
                                </Grid>
                            </Grid>
                        </Grid>
                        <Grid item xs={1} md={1} lg={1}></Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <CodeBrowser
                        context={{class: selectedClass? selectedClass.name : null}}
                        styles={styles}
                        class={selectedClass}
                        method={selectedMethod}
                        onMethodCompiled={this.methodCompiled}
                        onClassDefined={this.classDefined}
                        onClassCommented={this.classCommented}/>
                </Grid>
            </Grid>
        )
    }
}

export default ClassBrowser;
