import React, { Component } from 'react';
import { Grid, Paper, Drawer, List, ListItem, ListItemIcon, ListItemText } from '@material-ui/core';
import InboxIcon from '@material-ui/icons/MoveToInbox';
import CustomTable from '../controls/CustomTable';
import Inspector from './Inspector';
import { IDEContext } from '../IDEContext';
import InspectorIcon from '../icons/InspectorIcon';
import WorkspaceIcon from '../icons/WorkspaceIcon';
import DebuggerIcon from '../icons/DebuggerIcon';
import clsx from 'clsx';

class ResourceBrowser extends Component {
    static contextType = IDEContext;
    constructor(props) {
        super(props);
        this.state = {
            selectedType: null,
            resources: [],
            selectedResource: null,
        };
    }

    typeSelected = async (type) => {
        var resources;
        try {
            switch (type) {
                case 'Objects':
                    resources = await this.context.api.getObjects();
                    break;
                case 'Workspaces':
                    resources = await this.context.api.getWorkspaces();
                    break;
                case 'Debuggers':
                    resources = await this.context.api.getDebuggers();
                    break;
                default:
            }
        }
        catch (error) {this.context.reportError(error)}
        this.setState({selectedType: type, resources: resources});
    }

    resourceSelected = (resource) => {
        this.setState({selectedResource: resource});
    }

    inspectObject = (object) => {
        if (object) {this.context.inspectObject(object)}
    }  

    unpinObject = async (object) => {
        try {
            await this.context.api.unpinObject(object.id);
            this.setState({resources: this.state.resources.filter(r => r.id !== object.id)});
        }
        catch(error) {this.context.reportError(error)}
    }   

    objectOptions() {
        return [
            {label: 'Inspect', action: this.inspectObject},
            {label: 'Unpin', action: this.unpinObject},
        ]
    }

    openWorkspace = (workspace) => {
        if (workspace) {this.context.openWorkspace(workspace.id)}
    }

    workspaceOptions() {
        return [
            {label: 'Open', action: this.openWorkspace},
        ]
    }

    openDebugger = (d) => {
        if (d) {this.context.openDebugger(d.id)}
    }

    debuggerOptions() {
        return [
            {label: 'Open', action: this.openDebugger},
        ]
    }

    menuOptions() {
        var options;
        switch (this.state.selectedType) {
            case 'Objects':
                options = this.objectOptions();
                break;
            case 'Workspaces':
                options = this.workspaceOptions();
                break;
            case 'Debuggers':
                options = this.debuggerOptions();
                break;
            default:
        }
        return options;
    }

    objectColumns() {
        return [
            {field: 'id', headerName: 'ID', align: 'left'},
            {field: 'class', headerName: 'Class', align: 'left', minWidth: 200},
            {field: 'printString', headerName: 'Print String', minWidth: 200, align: 'left'},
        ];
    }

    workspaceColumns() {
        return [
            {field: 'id', headerName: 'ID', align: 'left'},
            {field: 'owner', headerName: 'Owner', align: 'center'},
        ]
    }

    debuggerColumns() {
        return [
            {field: 'id', headerName: 'ID', align: 'left'},
            {field: 'creator', headerName: 'Creator', align: 'center'},
            {field: 'description', headerName: 'Description', align: 'left', minWidth: 200},
        ]
    }

    resourceColumns(type) {
        var columns;
        switch (type) {
            case 'Objects':
                columns = this.objectColumns();
                break;
            case 'Workspaces':
                columns = this.workspaceColumns();
                break;
            case 'Debuggers':
                columns = this.debuggerColumns();
                break;
            default:
        }
        return columns;
    }

    render() {
        const {selectedType, resources, selectedResource} = this.state;
        const rows = resources;
        const columns = this.resourceColumns(selectedType);
        const styles = this.props.styles;
        const ow = selectedResource && selectedType === 'object'? 6 : 10;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={2} md={2} lg={2}>
                    <List>
                        {['Objects', 'Workspaces', 'Debuggers'].map(type =>
                            <ListItem
                                button
                                key={type}
                                selected={type===selectedType}
                                onClick={event => this.typeSelected(type)}>
                                <ListItemIcon>
                                    {type === 'Objects'? <InspectorIcon/> : type === 'Workspaces'? <WorkspaceIcon/> : <DebuggerIcon/>}
                                </ListItemIcon>
                                <ListItemText primary={type}/>
                            </ListItem>    
                        )}
                    </List>
                </Grid>
                <Grid item xs={ow} md={ow} lg={ow}>
                    {selectedType && <Paper className={fixedHeightPaper} variant="outlined">      
                        <CustomTable
                            styles={styles}
                            columns={columns}
                            rows={rows}
                            onSelect={this.resourceSelected}
                            menuOptions={this.menuOptions()}/>
                    </Paper>}
                </Grid>
                {selectedResource && selectedType === 'object' && <Grid item xs={4} md={4} lg={4}>
                    <Paper variant="outlined">
                        <Inspector
                            styles={styles}
                            root={selectedResource}
                            showWorkspace={false}/>
                        </Paper>
                </Grid>}
            </Grid>
        )
    }
}

export default ResourceBrowser;
