import React, { Component } from 'react';
import {
    TableContainer,
    Table,
    TableHead,
    TableBody,
    TableRow,
    TableCell } from '@material-ui/core';
import Scrollable from './Scrollable';

class CustomTable extends Component {
    constructor(props) {
        super(props);
        this.state = {
            selectedRow: null
        }
    }

    rowSelected = (event, row) => {
        this.setState({selectedRow: row});
        const handler = this.props.onSelect;
        if (handler) {handler(row)}
    }
    
    render() {
        const columns = this.props.columns;
        const rows = this.props.rows;
        return (
            <Scrollable>
                <TableContainer className={this.props.styles.container}>
                    <Table stickyHeader size="small">
                        <TableHead>
                            <TableRow>
                                {columns.map((column) => (
                                    <TableCell
                                        key={column.id}
                                        align={column.align}
                                        style={{minWidth: column.minWith}}>
                                        {column.label}
                                    </TableCell>
                                ))}
                            </TableRow>
                        </TableHead>
                        <TableBody>
                            {rows.map((row, index) => {
                                return (
                                    <TableRow
                                        hover
                                        tabIndex={-1}
                                        key={index}
                                        selected={row === this.state.selectedRow}
                                        onClick={event => this.rowSelected(event, row)}>
                                            {columns.map((column) => {
                                                const value = row[column.id];
                                                return (
                                                    <TableCell key={column.id} align={column.align}>
                                                        {column.format ? column.format(value) : value}
                                                    </TableCell>)
                                                })}
                                    </TableRow>)
                                })}
                        </TableBody>
                    </Table>
                </TableContainer>
            </Scrollable>
        )
    }
}

export default CustomTable;