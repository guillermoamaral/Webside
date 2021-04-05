import React, { Component } from 'react';
import { Grid, Paper, Drawer, List, ListItem, ListItemIcon, ListItemText } from '@material-ui/core';
import InboxIcon from '@material-ui/icons/MoveToInbox';
import CustomTable from '../controls/CustomTable';
import Inspector from './Inspector';
import { IDEContext } from '../IDEContext';
import InspectorIcon from '../icons/InspectorIcon';
import WorkspaceIcon from '../icons/WorkspaceIcon';
import DebuggerIcon from '../icons/DebuggerIcon';

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

    unpinObject = async (object) => {
        try {
            await this.context.api.unpinObject(object.id);
        }
        catch(error) {this.context.reportError(error)}
    }

    objectOptions() {
        return [
            {label: 'Unpin', action: this.unpinObject},
        ]
    }

    menuOptions() {
        var options;
        switch (this.state.selectedType) {
            case 'object':
                options = this.objectOptions();
                break;
            case 'debugger':
                options = this.debuggerOptions();
                break;
            case 'workspace':
                options = this.workspaceOptions();
                break;
            default:
        }
    }

    resourceColumns() {
        return [
            {id: 'id', label: 'ID', align: 'left'},
            {id: 'printString', label: 'Print String', minWidth: 200, align: 'left'},
        ];
    }

    render() {
        const {selectedType, resources, selectedResource} = this.state;
        const rows = resources;
        const columns = this.resourceColumns();
        const styles = this.props.styles;
        const ow = selectedResource && selectedType === 'object'? 6 : 10;
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
                    {selectedType && <Paper variant="outlined">      
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
