import React, { Component } from 'react';
import { IDEContext } from '../IDEContext';
import { Grid } from '@material-ui/core';
import { Line } from 'react-chartjs-2';

class MemoryStats extends Component {
    static contextType = IDEContext;
    constructor(props) {
        super(props);
        this.state = {
            loading: true,
            stats: [],
        }
    }

    componentDidMount() {
        this.setTimer();
    }

    componentWillUnmount() {
        clearTimeout(this.timer);
    }

    setTimer() {
        this.timer = setTimeout(() => {this.updateStats()}, 1000);
    }

    async updateStats() {
        try {
            const stats = await this.context.api.getMemoryStats(100);
            this.setState({stats: stats, loading: false});
            this.setTimer();
        }
        catch (error) {this.context.reportError(error)}
    }

    render() {
        const {stats} = this.state;
        const options = {
            legend: {
                position: 'bottom',
            },
            scales: {
                yAxes: [
                    {
                        type: 'linear',
                        display: true,
                        position: 'left',
                        id: 'size',
                        title: {
                            text: 'KB',
                            display: true,
                        }
                    },
                    {
                        type: 'linear',
                        display: true,
                        position: 'right',
                        id: 'number',
                        title: {
                            text: '#',
                            display: true,
                        }
                    },
                    {
                        type: 'linear',
                        display: true,
                        position: 'right',
                        id: 'percent',
                        title: {
                            text: '%',
                            display: true,
                        }
                    },
                    {
                        type: 'linear',
                        display: true,
                        position: 'right',
                        id: 'time',
                        title: {
                            text: 'ms',
                            display: true,
                        }
                    },
                ],
            },
        };
        const data = {
            labels: stats.map((s, i) => i.toString()),
            datasets: [
                {
                    label: 'Time',
                    data: stats.map(s => s.time),
                    fill: false,
                    borderColor: 'rgb(136, 247, 226)',
                    yAxisID: 'time',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Old Space Size',
                    data: stats.map(s => s.oldSize),
                    fill: false,
                    borderColor: 'rgb(68, 212, 146)',
                    yAxisID: 'size',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Young Space Size',
                    data: stats.map(s => s.youngSize),
                    fill: false,
                    borderColor: 'rgb(245, 235, 103)',
                    yAxisID: 'size',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Cradled Size',
                    data: stats.map(s => s.cradled),
                    fill: false,
                    borderColor: 'rgb(255, 161, 92)',
                    yAxisID: 'size',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Tenured Size',
                    data: stats.map(s => s.tenured),
                    fill: false,
                    borderColor: 'rgb(250, 35, 62)',
                    yAxisID: 'size',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Survival Rate',
                    data: stats.map(s => s.survivalRate),
                    fill: false,
                    borderColor: 'rgb(191, 38, 105)',
                    yAxisID: 'percent',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Remembered Before GC',
                    data: stats.map(s => s.rememberedBefore),
                    fill: false,
                    borderColor: 'rgb(112, 42, 140)',
                    yAxisID: 'number',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Remembered After GC',
                    data: stats.map(s => s.rememberedAfter),
                    fill: false,
                    borderColor: 'rgb(255, 115, 38)',
                    yAxisID: 'number',
                    borderWidth: 1,
                    pointRadius: 0,
                },
                {
                    label: 'Rescued',
                    data: stats.map(s => s.rescued),
                    fill: false,
                    borderColor: 'rgb(255, 115, 38)',
                    yAxisID: 'number',
                    borderWidth: 1,
                    pointRadius: 0,
                },
            ]
        };
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Line
                        height={80}
                        data={data}
                        options={options}/>
                </Grid>
            </Grid>
        )
    }
}

export default MemoryStats;
