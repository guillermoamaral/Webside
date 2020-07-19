import React, { Component } from 'react';
import CustomList from './CustomList';

class VariableList extends Component {
    render() {
        return (
            <CustomList
                itemLabel="name"
                items={this.props.variables}
                selectedItem={this.props.selectedVariable}
                onSelect={this.props.onSelect}
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