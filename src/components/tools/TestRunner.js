import React, { Component } from 'react';
import { AppContext } from '../../AppContext';
import LinearProgress from '@material-ui/core/LinearProgress';
import { Typography, Grid, Box, Paper, Button } from '@material-ui/core';
import { Alert } from '@material-ui/lab';
import RunIcon from '@material-ui/icons/PlayArrow';
import StopIcon from '@material-ui/icons/Stop';
import CustomTable from '../controls/CustomTable';

class TestRunner extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            status: {
                total: 0,
                running: false,
                current: null,
                results: {
                    run: 0,
                    passed: 0,
                    failed: 0,
                    errors: 0,
                    skipped: 0,
                    knownIssues: 0,
                }
            }
        }
    }

    componentDidMount() {
        this.updateStatus()
    }

    async runClicked() {
        this.setState({status: {
            total: 0,
            running: true,
            current: null,
            results: {
                run: 0,
                passed: 0,
                failed: 0,
                errors: 0,
                skipped: 0,
                knownIssues: 0,
            }}});        
        await this.context.api.runTestRun(this.props.id);
        this.updateStatus();        
    }

    async stopClicked() {
        const status = await this.context.api.stopTestRun(this.props.id);
        this.updateStatus();
    }

    async updateStatus() {
        if (this.state.updating) {return}
        this.setState({updating: true});
        const status = await this.context.api.getTestRunStatus(this.props.id);
        if (status.running) {setTimeout(() => {this.updateStatus()}, 1000)}
        this.setState({status: status, updating: false});
    }

    render() {
        const {total, running, current} = this.state.status;
        const results = this.state.status.results;
        const percent = total > 0? results.run / total * 100 : 0;
        const rows = [
            {type: 'Passed', count: results.passed},
            {type: 'Failed', count: results.failed},
            {type: 'Errors', count: results.errors},
            {type: 'Skipped', count: results.skipped},
            {type: 'Known Issues', count: results.knownIssues},
        ];
        const columns = [
            {id: 'type', label: 'Type', minWidth: 170, align: 'right'},
            {id: 'count', label: 'Count', minWidth: 100, align: 'center'}
        ];
        return (
            <div>
                <Grid container spacing={1} display="flex" alignItems="center">
                    <Grid item xs={1} md={1} lg={1}>
                        <Button
                            variant="outlined"
                            color={running? "secondary" : "primary"}
                            onClick={() => running? this.stopClicked() : this.runClicked()}>
                                {running? 'Stop' : 'Run'}
                        </Button>
                    </Grid>
                    <Grid item xs={9} md={9} lg={9} >
                        {running && <LinearProgress variant="determinate" value={percent}/>}
                    </Grid>
                    <Grid item xs={2} md={2} lg={2}>
                        {running && <Typography variant="subtitle1" color="textSecondary">
                            ({Math.round(percent) + '%'})
                        </Typography>}
                    </Grid>
                    <Grid item xs={4} md={4} lg={4}>
                        <Paper variant="outlined">
                            <CustomTable
                                styles={this.props.styles}
                                columns={columns}
                                rows={rows}>
                            </CustomTable>
                        </Paper>
                    </Grid>
                </Grid>
            </div>
        )
    }
}

export default TestRunner;