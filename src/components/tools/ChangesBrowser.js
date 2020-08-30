import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import CustomTable from '../controls/CustomTable';
import CodeEditor from '../parts/CodeEditor';
import clsx from 'clsx';
import { AppContext } from '../../AppContext';

class ChangesBrowser extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            selectedChange: null
        }
    }

    changeSelected = (change) => {
        this.setState({selectedChange: change});
    }

    menuOptions() {
        return [
            {label: 'Browse', action: ch => this.context.browseClass(ch.class)},
        ]
    }

    render() {
        const change = this.state.selectedChange;
        const rows = this.props.changes;
        const columns = [
            {id: 'type', label: 'Type', minWidth: 170, align: 'left'},
            {id: 'label', label: 'Target', minWidth: 100, align: 'left'},
            {id: 'project', label: 'Project', minWidth: 170, align: 'left'},
            {id: 'author', label: 'Author', minWidth: 100, align: 'center'},
            {
              id: 'timestamp',
              label: 'Timestamp',
              minWidth: 170,
              align: 'left',
              format: (value) => value.toLocaleString('en-US'),
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
                            menuOptions={this.menuOptions()}>
                        </CustomTable>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <CodeEditor
                        context={{class: change? change.class : null}}
                        styles={this.props.styles}
                        lineNumbers
                        source={change? change.sourceCode : ''}
                        showAccept={false}/>
                </Grid> 
            </Grid>
        )
    }
}

export default ChangesBrowser;