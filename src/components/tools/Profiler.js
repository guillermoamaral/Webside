import React, { Component } from 'react';
import { AppContext } from '../../AppContext';
import { FlameGraph } from 'react-flame-graph';
import { Grid, LinearProgress } from '@material-ui/core';
import CodeEditor from '../parts/CodeEditor';
import AutoSizedFlameGraph from '../controls/AutoSizedFlameGraph';

class Profiler extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            loading: true,
            results: null,
            selectedMethod: null
        }
    }

    componentDidMount() {
        this.updateResults()
    }

    async updateResults() {
        const results = await this.context.api.getProfilerResults(this.props.id);
        this.setState({results: results, loading: false})
    }

    async nodeChange(node) {
        const signature = node.name.split('>>');
        console.log(signature)
        const method = await this.context.api.getMethod(signature[0], signature[1]);
        this.setState({selectedMethod: method})
    }

    render() {
        const {loading, results, selectedMethod} = this.state;
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    {loading && <LinearProgress variant="indeterminate"/>}
                    {!loading &&
                        <AutoSizedFlameGraph
                            data={results}
                            height={300}
                            disableScroll
                            onChange={node => this.nodeChange(node)}/>}
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

