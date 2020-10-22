import React, { Component } from 'react';
import { Grid, Paper, IconButton, Tooltip } from '@material-ui/core';
import clsx from 'clsx';
import { Icon } from "@iconify/react";
import RestartIcon from '@iconify/icons-mdi/replay';
import HopIcon from '@iconify/icons-mdi/debug-step-into';
import SkipIcon from '@iconify/icons-mdi/debug-step-over';
import ResumeIcon from '@iconify/icons-mdi/play';
import TerminateIcon from '@iconify/icons-mdi/stop';
import { AppContext } from '../../AppContext';
import CustomList from '../controls/CustomList';
import CustomTable from '../controls/CustomTable';
import NativeCodeEditor from '../parts/NativeCodeEditor';

class NativeDebugger extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            frames: [],
            selectedFrame: null,
            registers: [],
        }
    }

    componentDidMount() {
        this.updateFramesAndRegisters();
    }

    async updateFramesAndRegisters() {
        const frames = await this.context.api.getNativeDebuggerFrames(this.props.id);
        let selected = null;
        if (frames.length > 0) {
            selected = frames[0];
            await this.updateFrame(selected)
        }
        const registers = await this.context.api.getNativeDebuggerRegisters(this.props.id);
        this.setState({
            frames: frames,
            selectedFrame: selected,
            registers: registers,
        });
    }

    frameSelected = async (frame) => {
        await this.updateFrame(frame);
        this.setState({selectedFrame: frame});
    }

    updateFrame = async (frame) => {
        if (!frame.method) {
            const info = await this.context.api.getNativeDebuggerFrame(this.props.id, frame.index);
            frame.code = info.code;
            frame.interval = info.interval;
        }
    }

    render() {
        const styles = this.props.styles; 
        const {frames, selectedFrame, registers} = this.state;
        const columns = [
            {id: 'name', label: 'Register', minWidth: 100, align: 'left'},
            {id: 'value', label: 'Value', minWidth: 100, align: 'right'},
        ];
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
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
                <Grid item xs={7} md={7} lg={7}>
                    <NativeCodeEditor
                        styles={styles}
                        lineNumbers={true}
                        source={selectedFrame? selectedFrame.code : ""}
                        selectedRanges={!this.props.selectedInterval? [] : [this.props.selectedInterval]}/>
                </Grid>
                <Grid item xs={5} md={5} lg={5}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={12} lg={12}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomTable
                                    styles={styles}
                                    columns={columns}
                                    rows={registers}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={12} lg={12}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    itemLabel="label"
                                    items={frames}
                                    selectedItem={selectedFrame}
                                    onSelect={this.frameSelected}/>
                            </Paper>
                        </Grid>
                    </Grid>
                </Grid> 
            </Grid>
        )
    }
}

export default NativeDebugger;
