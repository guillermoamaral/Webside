import React, { Component, Fragment } from 'react';
import { IDEContext } from '../IDEContext';
import { Grid, LinearProgress } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import CodeEditor from '../parts/CodeEditor';
import AutoSizer from 'react-virtualized-auto-sizer';
import { FlameGraph } from 'react-flame-graph';
import { HorizontalBar } from 'react-chartjs-2';

class Profiler extends Component {
    static contextType = IDEContext;
    constructor(props) {
        super(props);
        this.state = {
            loading: true,
            tree: null,
            ranking: null,
            selectedMode: 'tree',
            selectedMethod: null
        }
    }

    componentDidMount() {
        this.updateResults();
    }

    async updateResults() {
        try {
            const tree = await this.context.api.getProfilerTreeResults(this.props.id);
            const ranking = await this.context.api.getProfilerRankingResults(this.props.id);
            this.setState({tree: tree, ranking: ranking, loading: false})
        }
        catch (error) {this.context.reportError(error)}
    }

    async methodSelected(signature) {
        var method;
        if (signature.indexOf('>>') === -1) {
            method = null 
        } else {
            const parts = signature.split(') ')[1].split('>>');
            try {
                method = await this.context.api.getMethod(parts[0], parts[1]);
            }
            catch (error) {this.context.reportError(error)}
        }
        this.setState({selectedMethod: method})
    }

    modeChanged = (event, mode) => {
        this.setState({selectedMode: mode})
    }

    rankingClicked(element) {
        if (element) {
            this.methodSelected(element._model.label);
        }
    }

    render() {
        const {loading, tree, ranking, selectedMode, selectedMethod} = this.state;
        var rankingData = {
            labels: ranking? ranking.map(m => m.name) : [],
            datasets: [{
              backgroundColor: 'rgba(254, 188, 56, 0.8)',
              borderColor: 'rgba(255, 255, 255, 0.8)',
              borderWidth: 1,
              hoverBackgroundColor: 'rgba(255, 143, 0, 1)',
              hoverBorderColor: 'white',
              data: ranking? ranking.map(m => m.value) : [],
            }]
          };
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <ToggleButtonGroup
                        label='primary'
                        value={selectedMode}
                        exclusive
                        onChange={this.modeChanged}>
                        <ToggleButton value='tree' variant='outlined' size='small'>
                            Execution Tree
                        </ToggleButton>
                        <ToggleButton value='ranking' variant='outlined' size='small'>
                            Ranking
                        </ToggleButton>
                    </ToggleButtonGroup>    
                </Grid>
                {loading &&
                    <Grid item xs={12} md={12} lg={12}>
                        <LinearProgress variant="indeterminate"/>
                    </Grid>}
                <Grid item xs={12} md={12} lg={12}>
                    {!loading && selectedMode === 'tree' &&
                        <div style={{height: 300}}>
                            <AutoSizer>
                                {({height: autoSizerHeight, width}) => (
                                    <Fragment>
                                        <FlameGraph
                                            data={tree}
                                            height={autoSizerHeight}
                                            width={width}
                                            onChange={node => this.methodSelected(node.name)}/>
                                    </Fragment>)}
                            </AutoSizer>
                        </div>}
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    {!loading && selectedMode === 'ranking' &&
                        <HorizontalBar
                            height={80}
                            onElementsClick={elems => {this.rankingClicked(elems[0])}}
                            data={rankingData}
                            options={{legend: {display: false}}}/>}
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    {!loading &&
                        <CodeEditor
                            styles={this.props.styles}
                            lineNumbers={true}
                            source={!selectedMethod? "" : selectedMethod.source}
                            showAccept={false}/>}
                </Grid>
            </Grid>
        )
    }
}

export default Profiler;
