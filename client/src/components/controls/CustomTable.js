import React, { Component } from 'react';
import {
    TableContainer,
    Table,
    TableHead,
    TableBody,
    TableRow,
    TableCell } from '@material-ui/core';
import PopupMenu from './PopupMenu';
import Scrollable from './Scrollable';

class CustomTable extends Component {
    constructor(props) {
        super(props);
        this.state = {
            menuOpen: false,
            menuPosition: {x: null, y: null},
            selectedRow: null
        }
    }

    rowSelected = (event, row) => {
        this.setState({selectedRow: row});
        const handler = this.props.onSelect;
        if (handler) {handler(row)}
    }
    
    openMenu = (event) => {
        event.preventDefault();
        this.setState({menuOpen: true, menuPosition: {x: event.clientX - 2, y: event.clientY - 4}})
    }
    
    closeMenu = () => {
        this.setState({menuOpen: false});
    }
    
    menuOptionClicked = (option) => {
        const selected = this.state.selectedRow;
        if (option.action) {option.action(selected)}
    }
    
    getCellValue = (row, column) => {
        const getter = column.field;  
        const value = typeof getter == "string"? row[getter] : getter(row);
        return column.valueFormatter? column.valueFormatter(value) : value;
    }

    render() {
        const columns = this.props.columns;
        const rows = this.props.rows || [];
        return (
            <Scrollable>
                <div>
                    <TableContainer className={this.props.styles.container}>
                        <Table stickyHeader size="small">
                            {!this.props.noHeaders && <TableHead>
                                <TableRow>
                                    {columns.map((column) => (
                                        <TableCell
                                            key={column.field}
                                            align={column.align}
                                            style={{minWidth: column.minWith}}>
                                                {column.headerName}
                                        </TableCell>
                                    ))}
                                </TableRow> 
                            </TableHead>}
                            <TableBody>
                                {rows.map((row, index) => {
                                    return (
                                        <TableRow
                                            hover
                                            tabIndex={-1}
                                            key={index}
                                            selected={row === this.state.selectedRow}
                                            onClick={event => this.rowSelected(event, row)}
                                            onContextMenu={this.openMenu}>
                                                {columns.map((column) => {
                                                    return (
                                                        <TableCell
                                                            key={column.field}
                                                            align={column.align}
                                                            style={{color: row.color || "default"}}>
                                                                {this.getCellValue(row, column)}
                                                        </TableCell>)
                                                    })}
                                        </TableRow>)
                                    })}
                            </TableBody>
                        </Table>
                    </TableContainer>
                    <PopupMenu
                        options={this.props.menuOptions}
                        open={this.state.menuOpen}
                        position={this.state.menuPosition}
                        onOptionClick={this.menuOptionClicked}
                        onClose={this.closeMenu}/>
                </div>
            </Scrollable>
        )
    }
}

export default CustomTable;