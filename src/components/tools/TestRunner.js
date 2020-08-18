import React, { Component } from 'react';
import { AppContext } from '../../AppContext';
import LinearProgress from '@material-ui/core/LinearProgress';
import {
    Accordion,
    AccordionSummary,
    AccordionDetails,
    Typography,
    Grid,
    Box,
    Paper,
    Button,
    Card,
    CardContent,
    CardActions } from '@material-ui/core';
import { Alert } from '@material-ui/lab';
import RunIcon from '@material-ui/icons/PlayArrow';
import StopIcon from '@material-ui/icons/Stop';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import CustomTable from '../controls/CustomTable';

class TestRunner extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            status: {
                name: '',
                total: 0,
                running: false,
                current: null,
                summary: {run: 0, passed: 0, failed: 0, errors: 0, skipped: 0, knownIssues: 0}
            },
            results: {
                updated: false,
                groups: {}
            },
            filter: null,
        }
    }

    componentDidMount() {
        this.updateStatus()
    }

    typeColor(type) {
        let color;
        switch (type) {
            case 'passed':
                color = '#28a745';
                break;
            case 'failed':
                color = '#ffc107';
                break;
            case 'error':
                color = '#dc3545';
                break;
            case 'skipped':
                color = '#6c757d';
                break;
            case 'knownIssue':
                color = '#007bff';
                break;
            default:
                color = 'default'
        }
        return color;
    }

    summaryLabels() {
        return [
            {type: 'passed', label: 'Passed'},
            {type: 'failed', label: 'Failed'},
            {type: 'error', label: 'Errors'},
            {type: 'skipped', label: 'Skipped'},
            {type: 'knownIssue', label: 'Known Issues'},
            {type: 'run', label: 'Run'}
        ]
    }
  
    async runClicked() {
        this.setState({
            status: {
                total: 0,
                running: true,
                current: null,
                summary: {run: 0, passed: 0, failed: 0, errors: 0, skipped: 0, knownIssues: 0}
            },
            results: {updated: false, groups: {}}
        });        
        await this.context.api.runTestRun(this.props.id);
        this.updateStatus();        
    }

    async stopClicked() {
        await this.context.api.stopTestRun(this.props.id);
        this.updateStatus();
        this.updateResults();
    }

    async updateStatus() {
        if (this.state.updating) {return}
        this.setState({updating: true});
        const status = await this.context.api.getTestRunStatus(this.props.id);
        if (status.running) {setTimeout(() => {this.updateStatus()}, 1000)}
        this.setState({status: status, updating: false});
        if (status.total === status.summary.run + status.summary.skipped) {this.updateResults()}
    }

    async updateResults() {
        if (this.state.results.updated) {return}
        const tests = await this.context.api.getTestRunResults(this.props.id);
        const groups = {};
        tests.forEach(t => {
            if (!groups[t.class]) {groups[t.class] = {summary: {}, tests: []}}
            groups[t.class].tests.push(t);
        })
        Object.keys(groups).forEach(c => {
            groups[c].tests.forEach(t => {
                groups[c].summary[t.type] = groups[c].summary[t.type] + 1 || 1;
            })
        })
        this.setState({results: {updated: true, groups: groups}});
    }

    filterTests(type) {
        const filter = type === 'run'? null : type;
        this.setState({filter: filter})
    }

    menuOptions() {
        return [
            {label: 'Debug', action: this.debugTest},
            {label: 'Implementors', action: test => this.context.browseImplementors(test.selector)},
            {label: 'Browse class', action: test => this.context.browseClass(test.class)},
        ]
    }

    debugTest = async (test) => {
        const id = await this.context.api.debugTest(this.props.id, test.class, test.selector);
        this.context.openDebugger(id, 'Debugging expression');
    }

    render() {
        const styles = this.props.styles;
        const {status, results} = this.state;
        const {total, running, current, summary} = status;
        const percent = total > 0? summary.run / total * 100 : 0;
        const groups = results.groups;
        const ranking = Object.keys(groups).sort((c1, c2) => {
            const n1 = groups[c1].summary.failed || 0 + groups[c1].summary.error || 0;
            const n2 = groups[c2].summary.failed || 0 + groups[c2].summary.error || 0;
            if (n1 > n2) {return 1}
            if (n1 < n2) {return -1}
            return 0;
        });
        const testColumns = [
            {id: 'selector', label: 'Selector', minWidth: 100, align: 'left'},
            {id: 'time', label: 'Time (ms)', minWidth: 100, align: 'right'}            
        ];
        return (
            <div>
                <Grid container spacing={1} display="flex" alignItems="center" justify="center">
                    <Grid item xs={12} md={12} lg={12}>
                        <Typography variant="h6">
                            Suite: {status.name}
                        </Typography>
                    </Grid>
                    <Grid item xs={10} md={10} lg={10} >
                        {running && <LinearProgress variant="determinate" value={percent}/>}
                    </Grid>
                    <Grid item xs={1} md={1} lg={1}>
                        {running && <Typography variant="subtitle1" color="textSecondary">
                            {summary.run} / {total}
                        </Typography>}
                    </Grid>
                    <Grid item xs={1} md={1} lg={1}>
                        <Button
                            variant="outlined"
                            color={running? "secondary" : "primary"}
                            onClick={() => running? this.stopClicked() : this.runClicked()}>
                                {running? 'Stop' : 'Run'}
                        </Button>
                    </Grid>
                    <Grid item xs={12} md={12} lg={12}>
                        {running && <Typography variant="subtitle1" color="textSecondary">
                            Running: {current? current.class + ': ' + current.selector: ''}
                        </Typography>}
                    </Grid>
                    <Grid item xs={12} md={12} lg={12}>
                        <Grid container spacing={1} justify="space-around" alignItems="center">
                            {this.summaryLabels().map(l => {
                                return (
                                    <Grid item xs={2} md={2} lg={2} key={"grid-" + l.type}>
                                        <Card id={"card-" + l.type}>
                                            <CardContent key={"card-content-" + l.type}>
                                                <Typography variant="h3" style={{color: this.typeColor(l.type)}}>
                                                    {summary[l.type] || 0}
                                                </Typography>
                                            </CardContent>
                                            <CardActions>
                                                <Button
                                                    key={"filter-button-" + l.type}
                                                    style={{color: this.typeColor(l.type)}}
                                                    onClick={(event) => this.filterTests(l.type)}>
                                                        {l.label}
                                                    </Button>
                                            </CardActions>
                                        </Card>
                                    </Grid>
                                )
                            })}
                        </Grid>
                    </Grid>
                    <Grid item xs={12} md={12} lg={12}>
                        {ranking.map(c => {
                            const classSummary = groups[c].summary;
                            const filter = this.state.filter;
                            const rows = groups[c].tests.filter(t => !filter || t.type === filter);
                            rows.forEach(r => {r.color = this.typeColor(r.type)});
                            return (
                                <Accordion key={c}>
                                    <AccordionSummary expandIcon={<ExpandMoreIcon />} id={c}>
                                        <Box display="flex" p={1}>
                                            <Box p={1} width="100%">
                                                <Typography>{c}</Typography>          
                                            </Box>
                                            {this.summaryLabels().map(l => {
                                                return (
                                                    <Box p={1} flexShrink={0} key={"box-" + l.type}>
                                                        <Typography style={{color: this.typeColor(l.type)}}>
                                                            {(classSummary[l.type] || 0) + ' ' + l.label}
                                                        </Typography>
                                                    </Box>   
                                                )
                                            })}
                                        </Box>
                                    </AccordionSummary>
                                    <Grid container>
                                        <Grid item xs={1} md={1} lg={1}/>
                                        <Grid item xs={11} md={11} lg={11}>
                                            <CustomTable
                                                styles={styles}
                                                columns={testColumns}
                                                rows={rows}
                                                menuOptions={this.menuOptions()}>
                                            </CustomTable>
                                        </Grid>
                                    </Grid>
                                </Accordion>
                            )
                        })}
                    </Grid>
                </Grid>
            </div>
        )
    }
}

export default TestRunner;