import React, { Component } from 'react';
import CustomList from '../controls/CustomList';

class VariableList extends Component {
    extendedVariables(variables) {
        let extended = [];
        if (variables !== undefined) {
            const groups = {};
            variables.forEach(v => {
                if (groups[v.class] === undefined) {groups[v.class] = []}
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
        if (handler !== undefined) {handler(selected)}
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
                menuOptions={[
                    {label: 'Add', action: this.addVariable},
                    {label: 'Rename', action: this.renameVariable},
                    {label: 'Remove', action: this.removeVariable},
                    {label: 'Move to superclass', action: this.moveVariableUp},
                    {label: 'Move to subclass', action: this.moveVariableDown}]}
            />
        )
    }
};

export default VariableList;