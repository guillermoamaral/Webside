import React, { Component } from 'react';
import { Box, Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';

import { AppContext } from '../AppContext';
import CustomTree from './CustomTree';

class Inspector extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        if (this.props.onClose !== undefined) {this.props.onClose.bind()};
        const root = this.props.root;
        root.name = 'self';
        root.path = '';
        this.state = {
            root: root,
            objectTree: [root],
            objects: {},
            selectedObject: root,
        }
    }

    componentDidMount() {
        this.updateVariables(this.state.root)
    }

    close = () => {
        this.props.onClose(this.props.root.id);
    }

    updateVariables = (object) => {
        if (object.variables !== undefined) {return object.variables}
        this.context.api.getInstanceVariables(object.class)
            .then(variables => {
                object.variables = [];
                variables.forEach(v => {
                    const path = object.path + '/' + v.name;
                    this.context.api.getVariable(this.props.root.id, path)
                        .then(variable => {
                            variable.name = v.name;
                            variable.path = path;
                            object.variables.push(variable);
                            const objects = this.state.objects;
                            objects[variable.id] = variable;
                            this.setState({objectTree: this.state.objectTree, objects: objects});
                        })
                        .catch(error => {})    
                });
            })
            .catch(error => {return []})
    }

    variableSelected = (object) => {
        this.setState({selectedObject: object})
    }

    variableExpanded = (object) => {
        if (object !== undefined) {
            object.variables.forEach(v => this.updateVariables(v))
        }
    }

    render() {
        const {objectTree, selectedObject} = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Box className={this.props.classes.box}>
                <Grid container spacing={1}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={6} lg={6}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomTree
                                    items={objectTree}
                                    itemLabel="name"
                                    id="id"
                                    children={"variables"}
                                    onExpand={this.variableExpanded}
                                    onSelect={this.variableSelected}
                                />
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={6} lg={6}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                {selectedObject.printString}
                            </Paper>
                        </Grid>
                    </Grid>
                </Grid>
            </Box>
        )
    };
}

export default Inspector;