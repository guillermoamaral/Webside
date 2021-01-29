import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import { IDEContext } from '../IDEContext';
import CodeMerge from '../parts/CodeMerge';

class MethodDifferences extends Component {
    static contextType = IDEContext;

    render() {
        const styles = this.props.styles;
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper variant="outlined">
                        <CodeMerge
                            styles={styles}
                            leftCode={this.props.leftMethod.source}
                            rightCode={this.props.rightMethod.source}/>
                    </Paper>
                </Grid>
            </Grid>
        )
    }
}

export default MethodDifferences;