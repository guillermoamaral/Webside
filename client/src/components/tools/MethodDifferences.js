import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';
import { IDEContext } from '../IDEContext';
import MethodList from '../parts/MethodList';
import CodeMerge from '../parts/CodeMerge';

class MethodDifferences extends Component {
    static contextType = IDEContext;

    render() {
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <CodeMerge
                        styles={styles}
                        leftCode={this.props.leftMethod.source}
                        rightCode={this.props.rightMethod.source}/>
                </Grid>
            </Grid>
        )
    }
}

export default MethodDifferences;