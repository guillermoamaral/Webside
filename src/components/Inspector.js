import React, { Component } from 'react';
import { Grid, Typography, Paper, IconButton } from '@material-ui/core';
import CloseIcon from '@material-ui/icons/Close';
import clsx from 'clsx';

import CustomTree from './CustomTree';

class Inspector extends Component {
    constructor(props) {
        super(props);
        this.reportError = props.onError.bind();
        const root = this.props.root;
        root.name = 'self';
        this.state = {
            root: root,
            objectTree: [root],
            selectedObject: root,
        }
    }

    componentDidMount() {
        this.getVariables(this.state.root)
    }

    close = () => {
        this.props.onClose.bind();
        this.props.onClose(this.props.root.id);
    }

    getVariables = (object) => {
        if (object.variables !== undefined) { return object.variables }
        this.props.api.instanceVariablesOf(object.class)
            .then(variables => {
                object.variables = [];
                variables.forEach(v => {
                    this.props.api.variableOf(object.id, v.name)
                        .then(variable => {
                            variable.name = v.name; 
                            object.variables.push(variable);
                            this.setState({objectTree: this.state.objectTree});
                        })
                        .catch(error => {})    
                });
            })
            .catch(error => { return [] })
    }

    variableSelected = (variable) => {}

    render() {
        const root = this.props.root;
        const { objectTree, selectedObject } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Typography
                        component="h6"
                        variant="h6"
                        color="inherit"
                        noWrap
                        className={this.props.classes.grow}
                    >
                        {this.props.root.class + ': ' + this.props.root.id}
                        <IconButton onClick={this.close} size="small" >
                            <CloseIcon fontSize="small"/>
                        </IconButton>
                    </Typography>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={6} md={6} lg={6}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomTree
                                    items={objectTree}
                                    label="name"
                                    children={"variables"}
                                    onSelect={this.variableSelected}
                                />
                            </Paper>
                        </Grid>
                        <Grid item xs={6} md={6} lg={6}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                {this.state.selectedObject.printString}
                            </Paper>
                        </Grid>
                    </Grid>
                </Grid>
            </Grid>
        )
    };
}

export default Inspector;