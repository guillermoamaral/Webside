import React, { Component } from 'react';
import { Grid, IconButton, Tooltip } from '@material-ui/core';
import { Icon } from "@iconify/react";
import RestartIcon from '@iconify/icons-mdi/replay';
import HopIcon from '@iconify/icons-mdi/debug-step-into';
import SkipIcon from '@iconify/icons-mdi/debug-step-over';
import ResumeIcon from '@iconify/icons-mdi/play';
import TerminateIcon from '@iconify/icons-mdi/stop';
import CustomList from '../controls/CustomList';
import NativeCodeEditor from '../parts/NativeCodeEditor';

class NativeDebugger extends Component {
    constructor(props) {
        super(props);
        this.state = {
            frames: [],
            selectedFrame: null,
        }
    }

    render() {
        const {selectedFrame} = this.state;
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    <Tooltip title="Hop" placement="top">
                        <IconButton color="inherit" onClick={this.hopClicked} size="medium">
                            <Icon icon={HopIcon}/>
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Skip" placement="top">
                        <IconButton color="inherit" onClick={this.skipClicked} size="medium">
                            <Icon icon={SkipIcon}/>
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Restart" placement="top">
                        <IconButton color="inherit" onClick={this.restartClicked} size="medium">
                            <Icon icon={RestartIcon}/>
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Resume" placement="top">
                        <IconButton color="inherit" onClick={this.resumeClicked} size="medium">
                            <Icon icon={ResumeIcon}/>
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Terminate" placement="top">
                        <IconButton color="inherit" onClick={this.terminateClicked} size="medium">
                            <Icon icon={TerminateIcon}/>
                        </IconButton>
                    </Tooltip>
                </Grid>
                <Grid item xs={8} md={8} lg={8}>
                    <NativeCodeEditor
                        styles={this.props.styles}
                        lineNumbers={true}
                        source={selectedFrame? selectedFrame.method : "000:\tpush rbp\r001:\tmov rbp, rsp"}
                        selectedRanges={!this.props.selectedInterval? [] : [this.props.selectedInterval]}/>
                </Grid>
                <Grid item xs={4} md={4} lg={4}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={12} lg={12}>
                            <CustomList></CustomList>
                        </Grid>
                        <Grid item xs={12} md={12} lg={12}>
                            <CustomList></CustomList>
                        </Grid>
                    </Grid>
                </Grid> 
            </Grid>
        )
    }
}

export default NativeDebugger;
