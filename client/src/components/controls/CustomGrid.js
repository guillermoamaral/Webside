import React, { Component } from 'react';
import { DataGrid } from '@material-ui/data-grid';
import PopupMenu from './PopupMenu';
import Scrollable from './Scrollable';

class CustomGrid extends Component {
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
        return column.format? column.format(value) : value;
    }

    render() {
        const columns = this.props.columns;
        columns.forEach(c => c.width = c.width || c.minWidth);
        const rows = this.props.rows || [];
        rows.forEach((r, i) => {r.id = r.id || i});
        const pageSize = this.props.pageSize || 5;
        return (
            <Scrollable>
                <div style={{height: 400}}>
                    <div style={{display: 'flex', height: '100%'}}>
                        <div style={{flexGrow: 1}}>
                            <DataGrid
                                rows={rows}
                                columns={columns}
                                pageSize={pageSize}
                                checkboxSelection/>
                            </div>
                    </div>
                </div>
                <PopupMenu
                    options={this.props.menuOptions}
                    open={this.state.menuOpen}
                    position={this.state.menuPosition}
                    onOptionClick={this.menuOptionClicked}
                    onClose={this.closeMenu}/>
            </Scrollable>
        )
    }
}

export default CustomGrid;