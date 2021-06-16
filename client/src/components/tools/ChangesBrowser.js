import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import CustomTable from '../controls/CustomTable';
import CustomGrid from '../controls/CustomGrid';
import CodeEditor from '../parts/CodeEditor';
import clsx from 'clsx';
import { IDEContext } from '../IDEContext';

class ChangesBrowser extends Component {
    static contextType = IDEContext;
    constructor(props) {
        super(props);
        this.state = {
            selectedChange: null
        }
    }

    changeSelected = (change) => {
        this.setState({selectedChange: change});
    }

    browseClass = (change) => {
        if (change) {this.context.browseClass(change.class)}
    }

    menuOptions() {
        return [
            {label: 'Browse', action: this.browseClass},
        ]
    }

    render() {
        const change = this.state.selectedChange;
        const rows = this.props.changes;
        const columns = [
            {field: 'type', headerName: 'Type', minWidth: 150, align: 'left'},
            {field: 'label', headerName: 'Target', minWidth: 250, align: 'left'},
            {field: 'project', headerName: 'Project', minWidth: 150, align: 'left'},
            {field: 'author', headerName: 'Author', minWidth: 150, align: 'center'},
            {
              field: 'timestamp',
              headerName: 'Timestamp',
              minWidth: 200,
              align: 'left',
              valueFormatter: (params) => {return params.value.toLocaleString('en-US')},
            },
        ];
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper className={fixedHeightPaper} variant="outlined">      
                        <CustomTable
                            styles={styles}
                            columns={columns}
                            rows={rows}
                            onSelect={this.changeSelected}
                            menuOptions={this.menuOptions()}/>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeEditor
                            context={{class: change? change.class : null}}
                            styles={this.props.styles}
                            lineNumbers
                            source={change? change.sourceCode : ''}
                            showAccept={false}/>
                    </Paper>
                </Grid> 
            </Grid>
        )
    }
}

export default ChangesBrowser;