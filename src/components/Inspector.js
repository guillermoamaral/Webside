import React, { Component } from 'react';
import { Box, Grid, Typography, Paper, IconButton } from '@material-ui/core';
import CloseIcon from '@material-ui/icons/Close';
import clsx from 'clsx';

import CustomTree from './CustomTree';

class Inspector extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        if (this.props.onClose !== undefined) { this.props.onClose.bind() };
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
        if (object.variables !== undefined) { return object.variables }
        this.props.api.instanceVariablesOf(object.class)
            .then(variables => {
                object.variables = [];
                variables.forEach(v => {
                    const path = object.path + '/' + v.name;
                    this.props.api.variableOf(this.props.root.id, path)
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
            .catch(error => { return [] })
    }

    variableSelected = (object) => {
        if (object !== undefined) {
            this.updateVariables(object);
            this.setState({selectedObject: object})
        }
    }

    variableExpanded = (object) => {
    }

    render() {
        const root = this.props.root;
        const { objectTree, selectedObject } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Box className={this.props.classes.box}>
                <Grid container spacing={1} justify="center">
                    {/* <Grid item xs={12} md={12} lg={12}>
                        <Typography
                            component="h6"
                            variant="h6"
                            color="inherit"
                            noWrap
                            className={this.props.classes.title}
                        >
                            {root.class + ': ' + root.id}
                            {this.props.onClose &&
                                <IconButton onClick={this.close} size="small" >
                                    <CloseIcon fontSize="small"/>
                                </IconButton>}
                        </Typography>
                    </Grid> */}
                    <Grid item xs={12} md={12} lg={12}>
                        <Grid container spacing={1}>
                            <Grid item xs={12} md={6} lg={6}>
                                <Paper className={fixedHeightPaper} variant="outlined">
                                    <CustomTree
                                        items={objectTree}
                                        label="name"
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
                </Grid>
            </Box>
        )
    };
}

export default Inspector;