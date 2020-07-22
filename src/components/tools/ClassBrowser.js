import React, { Component } from 'react';
import {
    Grid,
    Box,
    Paper,
    RadioGroup,
    FormControlLabel,
    Radio
} from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import clsx from 'clsx';

import { AppContext } from '../../AppContext';
import SearchList from '../parts/SearchList';
import ClassTree from '../parts/ClassTree';
import VariableList from '../parts/VariableList';
import CategoryList from '../parts/CategoryList';
import MethodList from '../parts/MethodList';
import CodeEditor from '../parts/CodeEditor';

class ClassBrowser extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            root: this.props.root,
            classes: {},
            classNames: [],
            selectedClass: null,
            selectedVariable: null,
            selectedCategory: null,
            selectedMethod: null,
            selectedSide: "instance",
            selectedMode: "definition",
        }
    }

    componentDidMount(){
        this.getClassNames();
        this.changeRoot(this.state.root);
    }

    getClassNames = () => {
        this.context.api.getClassNames()
            .then(names => {this.setState({classNames: names})})
            .catch(error => {})
    }

    changeRoot = async (classname) => {
        const tree = await this.context.api.getClassTree(classname, 3);
        const species = tree[0];
        this.setState(
            {root: classname, classes: {[classname]: species}},
            () => {this.classSelected(species)}
        )
    }

    currentSelections() {
        return {
            class: this.state.selectedClass,
            variable: this.state.selectedVariable,
            category: this.state.selectedCategory,
            method: this.state.selectedMethod,
            mode: this.state.selectedMode,
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
                selectedMethod: selections.method,
                selectedMode: selections.mode,         
            }
        })
    }

    // Contents..
    currentVariables() {
        const species = this.state.selectedClass;
        return (species == null || species.variables === undefined)? [] : species.variables;
    }

    currentCategories = () => {
        const species = this.state.selectedClass;
        return (species == null || species.categories === undefined)? [] : species.categories;
    }

    currentMethods = () => {
        const species = this.state.selectedClass;
        const category = this.state.selectedCategory;
        const variable = this.state.selectedVariable;
        if (species == null) {return []}
        if (category == null && variable === null) {return species.methods}
        if (category == null) {return species[variable.name]}
        if (variable == null) {
            return species.methods.filter(m => {return m.category === category});
        }
        return species[variable.name].filter(m => {return m.category === category});
    }

    currentSource = () => {
        const {selectedMode, selectedClass, selectedMethod} = this.state;
        let source;
        switch (selectedMode) {
            case "comment":
                source = selectedClass === null ? '' : selectedClass.comment;
                break;
            case "definition":
                source = selectedClass === null ? '' : selectedClass.definition;
                break;
            case "source":    
                source = selectedMethod === null ? '' : selectedMethod.source;
                break;
            default:
        }
        return source
    }

    // Updating...
    async updateClass(selections, force = false) {
        const species = selections.class;
        if (force || species.definition === undefined) {
            const definition = await this.context.api.getClass(species.name);
            species.definition = definition.definition;
            species.comment = definition.comment;
            species.superclass = definition.superclass;
        }
        if (force || species.subclasses === undefined) {
            species.subclasses = await this.context.api.getSubclasses(species.name);
        }
    }

    async updateSubclasses(species) { 
        if (species.subclasses !== undefined) {
            await Promise.all(species.subclasses.map (async c => {
                if (c.subclasses === undefined) {
                    c.subclasses = await this.context.api.getSubclasses(c.name);
                }
            }))
        }
    }
    
    async updateVariables(selections, force = false) {
        const species = selections.class;
        if (force || species.variables === undefined) {
            species.variables = await this.context.api.getVariables(species.name);
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
            const categories = await this.context.api.getCategories(species.name);
            species.categories = categories.sort();
        }
        if (!species.categories.includes(selections.category)) {
            selections.category = null;
        }
    }

    async updateMethods(selections, force = false) {
        const species = selections.class;
        if (force || species.methods === undefined) {
            const methods = await this.context.api.getMethods(species.name);
            species.methods = methods.sort((a, b) => {return a.selector <= b.selector? -1 : 1});
        }
        const variable = selections.variable;
        if (variable !== null && (force || species[variable.name] === undefined)) {
            species[variable.name] = await this.context.api.getMethodsUsing(species.name, variable.name);            
        }
        var method = selections.method;
        if (method !== null) {
            method = species.methods.find(m => {return m.selector === method.selector});
            selections.method = method === undefined ? null : method;    
        }
    }

    async updateMethod(selections, force = true) {
        const species = selections.class;
        if (force) {
            const method = await this.context.api.getMethod(species.name, selections.method.selector);
            if (method !== null) { 
                species.methods = species.methods.map(m => {return m.selector === method.selector? method : m})
                selections.method = method;
            }
        }
    }

    // Events...
    classSelected = async (species) => {
        const selections = this.currentSelections();
        selections.class = species;
        await this.updateClass(selections);
        await this.updateSubclasses(species);
        await this.updateVariables(selections);
        await this.updateCategories(selections);
        await this.updateMethods(selections);
        if (selections.method === null) {
            selections.mode = "definition"
        }
        this.applySelections(selections)
    }

    classExpanded = async (species) => {
        await this.updateSubclasses(species);
        this.setState({classes: this.state.classes});
    }

    defineClass = async (definition) => {
        const species = await this.context.api.defineClass(this.state.selectedClass.name, definition);    
        const classes = this.state.classes;
        const current = classes[species.name];
        if (current !== undefined) {
            current.definition = species.definition;
            this.classSelected(current);
        } else {
            classes[species.name] = species;
            const superclass = classes[species.superclass];
            if (superclass !== undefined) {
                superclass.subclasses.push(species);
                superclass.subclasses.sort((a, b) => {return a.name <= b.name? -1 : 1});
            }
            this.classSelected(species);
        }
    }
    
    commentClass = async (comment) => {
        const species = await this.context.api.commentClass(this.state.selectedClass.name, comment);
        const classes = this.state.classes;
        classes[species.name].comment = species.comment;
        this.setState({classes: classes})
    }
    
    classRemoved = (species) => {
        const classes = this.state.classes;
        delete classes[species.name];
        const superclass = classes[species.superclass];
        if (superclass !== undefined) {
            superclass.subclasses = superclass.subclasses.filter(c => {return c !== species});
            this.classSelected(superclass);
        } else {
            this.changeRoot('Object')
        }
    }

    variableSelected = async (variable) => {
        const selections = this.currentSelections();
        selections.variable = variable;
        await this.updateMethods(selections);
        this.applySelections(selections);
    }

    categorySelected = async (category) => {
        const selections = this.currentSelections();
        selections.category = category;
        await this.updateMethods(selections);
        this.applySelections(selections);
    }

    categoryRemoved = async (category) => {        
        const selections = this.currentSelections();
        selections.category = null;
        await this.updateMethods(selections);
        this.applySelections(selections);
    }    

    methodSelected = async (method) => {
        const selections = this.currentSelections();
        selections.method = method;
        await this.updateMethod(selections);
        selections.mode = "source";
        this.applySelections(selections);
    }

    methodRemoved = (method) => {
        const classes = this.state.classes;
        classes[method.class].methods = classes[method.class].methods.filter(m => {return m.selector !== method.selector});
        this.setState({classes: classes})
    }

    sideChanged = (event, side) => {
        if (side == null) return;
        this.setState({selectedSide: side});
        if (side === "instance") {
            const name = this.state.root;
            this.changeRoot(name.slice(0, name.length - 6))
        } else {
            this.changeRoot(this.state.root + " class")
        }
    }

    compileMethod = async (source) => {
        const species = this.state.selectedClass;
        const category = this.state.selectedCategory;
        const method = await this.context.api.compileMethod(species.name, category, source);
        const selections = this.currentSelections();
        if (!species.categories.includes(method.category)) {
            await this.updateCategories(selections, true);
        }
        selections.category = method.category;
        const methods = species.methods;
        if (methods === undefined ||
            methods.find(m => {return m.selector === method.selector}) === undefined) {
            await this.updateMethods(selections, true);
        }
        selections.method = species.methods.find(m => {return m.selector === method.selector});
        this.applySelections(selections)  
    }

    newMethod = () => {
        const template = {
            class: this.state.selectedClass !== null? this.state.selectedClass.name : null,
            category: this.state.selectedCategory,
            source: 'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements'
        }
        this.setState({selectedMethod: template})
    }

    modeChanged = (event, mode) => {
        this.setState({selectedMode: mode})
    }

    saveClicked = (source) => {
        switch (this.state.selectedMode) {
            case "comment":
                this.commentClass(source);
                break;
            case "definition":
                this.defineClass(source);
                break;
            case "source":    
                this.compileMethod(source);
                break;
            default:
        }
    }

    render() {
        const {
            root,
            classes,
            selectedSide,
            selectedClass,
            selectedVariable,
            selectedCategory,
            selectedMethod,
            selectedMode} = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={0}>
                        <Grid item xs={11} md={11} lg={11}>
                            <Grid item xs={12} md={12} lg={12}>
                                <Grid container spacing={1}>
                                    <Grid item xs={3} md={3} lg={3}>
                                        <SearchList
                                            options={this.state.classNames}
                                            onChange={classname => {this.changeRoot(classname)}}/>
                                    </Grid>                        
                                    <Grid item xs={3} md={3} lg={3}>
                                    </Grid>
                                    <Grid item xs={3} md={3} lg={3}>
                                        <Box display="flex" justifyContent="center">
                                            <RadioGroup
                                                name="side"
                                                value={selectedSide}
                                                onChange={this.sideChanged}
                                                defaultValue="instance"
                                                row
                                                >
                                                <FormControlLabel value="instance" control={<Radio size="small" color="primary"/>} label="Instance"/>
                                                <FormControlLabel value="class" control={<Radio size="small" color="primary"/>} label="Class" />
                                            </RadioGroup>
                                        </Box>
                                    </Grid>
                                    <Grid item xs={3} md={3} lg={3}>
                                    </Grid>                            
                                </Grid>
                            </Grid>
                            <Grid item xs={12} md={12} lg={12}>
                                <Grid container spacing={1}>
                                    <Grid item xs={12} md={3} lg={3}>
                                        <Paper className={fixedHeightPaper} variant="outlined">
                                            <ClassTree
                                                root={classes[root]}
                                                selectedClass={selectedClass}
                                                onExpand={this.classExpanded}
                                                onSelect={this.classSelected}
                                                onRemoved={this.classRemoved}/>
                                        </Paper>
                                    </Grid>
                                    <Grid item xs={12} md={3} lg={3}>
                                        <Paper className={fixedHeightPaper} variant="outlined">
                                            <VariableList
                                                variables={this.currentVariables()}
                                                selectedVariable={selectedVariable}
                                                onSelect={this.variableSelected}/>
                                        </Paper>
                                    </Grid>
                                    <Grid item xs={12} md={3} lg={3}>
                                        <Paper className={fixedHeightPaper} variant="outlined">
                                            <CategoryList
                                                class={selectedClass}
                                                categories={this.currentCategories()}
                                                selectedCategory={selectedCategory}
                                                onSelect={this.categorySelected}
                                                onRemoved={this.categoryRemoved}/>
                                        </Paper>
                                    </Grid>
                                    <Grid item xs={12} md={3} lg={3}>
                                        <Paper className={fixedHeightPaper} variant="outlined">
                                            <MethodList
                                                menuOptions={[{label: 'New', action: this.newMethod}]}
                                                methods={this.currentMethods()}
                                                selectedMethod={selectedMethod}
                                                onSelect={this.methodSelected}
                                                onRemoved={this.methodRemoved}
                                                />
                                        </Paper>
                                    </Grid>
                                </Grid>
                            </Grid>
                        </Grid>
                        <Grid item xs={1} md={1} lg={1}></Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={12} lg={12}>
                            <ToggleButtonGroup
                                label="primary"
                                value={selectedMode}
                                exclusive
                                onChange={this.modeChanged}>
                                <ToggleButton value="source" variant="outlined" size="small">
                                    Method defintion
                                </ToggleButton>
                                <ToggleButton value="definition" variant="outlined" size="small">
                                    Class definition
                                </ToggleButton>
                                <ToggleButton value="comment" variant="outlined" size="small">
                                    Class comment
                                </ToggleButton>
                            </ToggleButtonGroup>    
                        </Grid>
                        <Grid item xs={12} md={12} lg={12}>
                            <CodeEditor
                                classes={this.props.classes}
                                source={this.currentSource()}
                                showAccept={true}
                                onAccept={this.saveClicked}
                                />
                        </Grid>
                    </Grid>
                </Grid>
            </Grid>
        )
    };
}

export default ClassBrowser;
