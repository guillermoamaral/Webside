import React, { Component } from 'react';
import CustomTable from '../controls/CustomTable';
import { IDEContext } from '../IDEContext';

class RegisterTable extends Component {
    static contextType = IDEContext;

    registerSelected = (frame) => {
        const handler = this.props.onSelect;
        if (handler) {handler(frame)}
    }

    menuOptions() {
        return [
            {label: 'Inspect', action: this.inspect},
        ]
    }

    inspect = (register) => {

    } 

    render() {
        const styles = this.props.styles;
        const columns = [
            {id: 'name', label: 'Register', align: 'left'},
            {id: 'value', label: 'Value', align: 'right'},
            {id: 'object', label: 'Object', align: 'left'},
        ];
        return (
            <CustomTable
                styles={styles}
                columns={columns}
                rows={this.props.registers}
                onSelect={this.registerSelected}
                menuOptions={this.menuOptions()}/>
        )
    }
}

export default RegisterTable;