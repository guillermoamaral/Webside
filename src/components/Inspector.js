import React, { Component } from 'react';
import { Paper } from '@material-ui/core';
import axios from 'axios';
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
            selectedObject: null,
        }
    }

    componentDidMount() {
        console.log(this.state)
        this.getVariables(this.state.root)
    }

    getVariables = (object) => {
        if (object.variables !== undefined) { return object.variables }
        axios.get(this.props.baseUri + '/classes/' + object.class + '/instance-variables')
            .then(res => {
                object.variables = [];
                res.data.forEach(v => {
                    axios.get(this.props.baseUri + '/objects/' + object.id + '/' + v.name)
                        .then(res => {
                            const variable = res.data;
                            variable.name = v.name; 
                            object.variables.push(variable)})
                        .catch(error => {this.reportError(error)})    
                });
                this.setState({objectTree: this.state.objectTree})
            })
            .catch(error => {this.reportError(error); return []})
    }

    variableSelected = (variable) => {}

    render() {
        const root = this.props.root;
        const { objectTree, selectedObject } = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Paper className={fixedHeightPaper} variant="outlined">
                <CustomTree
                    items={objectTree}
                    label="name"
                    children={"variables"}
                    onSelect={this.variableSelected}
                />
            </Paper>
        )
    };
}

export default Inspector;