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

    inspect = async (register) => {
        try {
            await this.context.api.pinNativeDebuggerRegister(this.props.debugger, register.name);
            const object = await this.context.api.getObject(register.name);
            this.context.inspectObject(object)
        }
        catch (error) {this.context.reportError(error)}
    } 

    render() {
        const styles = this.props.styles;
        const columns = [
            {field: 'name', headerName: 'Register', align: 'left'},
            {field: r => {return r.value.toString(16).toUpperCase()}, headerName: 'Native', align: 'right'},
            {field: 'object', headerName: 'Object', align: 'left'},
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