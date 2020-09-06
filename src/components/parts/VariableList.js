import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { AppContext } from '../../AppContext';
import { withDialog } from '../dialogs';

class VariableList extends Component {
    static contextType = AppContext;

    extendedVariables(variables) {
        let extended = [];
        if (variables) {
            const groups = {};
            variables.forEach(v => {
                if (!groups[v.class]) {groups[v.class] = []}
                groups[v.class].push(v);
            })
            Object.keys(groups).forEach(c => {
                extended.push({name: c, type: 'separator'});
                groups[c].forEach(v => extended.push(v))
            })
        }
        return extended;
    }

    variableSelected = (variable) => {
        const selected = variable.type === 'separator' ? null : variable;
        const handler = this.props.onSelect;
        if (handler) {handler(selected)}
    }

    addVariable = async () => {
        try {
            const name = await this.props.dialog.prompt('New variable');
            const variable = await this.context.api.addInstanceVariable(this.props.class.name, name);
            if (variable && this.props.onAdd) {this.props.onAdd(variable)}
        }
        catch (error) {console.log(error)}
    }

    renameVariable = async (variable) => {
        if (!variable) {return}
        try {
            const newName = await this.props.dialog.prompt({title: 'Rename variable', defaultValue: variable.name});
            await this.context.api.renameInstanceVariable(this.props.class.name, variable.name, newName);
            variable.name = newName;
            if (this.props.onRename) {this.props.onRename(variable, newName)}
        }
        catch (error) {console.log(error)}
    }

    removeVariable = async (variable) => {
        if (!variable) {return}
        await this.context.api.deleteInstanceVariable(this.props.class.name, variable.name);
        if (this.props.onRemove) {this.props.onRemove(variable)}
    }

    menuOptions() {
        return [
            {label: 'Add', action: this.addVariable},
            {label: 'Rename', action: this.renameVariable},
            {label: 'Remove', action: this.removeVariable},
            {label: 'Move to superclass', action: this.moveVariableUp},
            {label: 'Move to subclass', action: this.moveVariableDown}
        ]
    }

    render() {
        const variables = this.extendedVariables(this.props.variables);
        return (
            <CustomList
                itemLabel="name"
                itemDivider={item => item.type === 'separator'}
                items={variables}
                selectedItem={this.props.selectedVariable}
                onSelect={this.variableSelected}
                menuOptions={this.menuOptions()}
            />
        )
    }
};

export default withDialog()(VariableList);