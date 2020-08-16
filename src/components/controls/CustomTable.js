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

    menuOptions() {
        if (this.props.menuOptions) {
          return this.props.menuOptions.map(option => {
            return !option? null : {
              label: option.label,
              action: () => {this.menuOptionClicked(option)}
            }
          })
        }
    }
    
    openMenu = (event) => {
        event.preventDefault();
        this.setState({menuOpen: true, menuPosition: {x: event.clientX - 2, y: event.clientY - 4}})
    };
    
    closeMenu = () => {
        this.setState({menuOpen: false});
    }
    
    menuOptionClicked(option) {
        if (option.action) {
          option.action(this.state.selectedRow);
        }
    }
    
    render() {
        const columns = this.props.columns;
        const rows = this.props.rows;
        return (
            //<Scrollable>
            <div>
                <TableContainer className={this.props.styles.container}>
                    <Table stickyHeader size="small">
                        {!this.props.noHeaders && <TableHead>
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
                                                const value = row[column.id];
                                                return (
                                                    <TableCell key={column.id} align={column.align} style={{color: row.color || "default"}}>
                                                        {column.format ? column.format(value) : value}
                                                    </TableCell>)
                                                })}
                                    </TableRow>)
                                })}
                        </TableBody>
                    </Table>
                </TableContainer>
                <PopupMenu
                    options={this.menuOptions()}
                    open={this.state.menuOpen}
                    position={this.state.menuPosition}
                    onClose={this.closeMenu}/>
            </div>
            //</Scrollable>
        )
    }
}

export default CustomTable;