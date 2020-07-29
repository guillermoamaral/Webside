import React, { Component } from 'react';
import { Grid, Paper, IconButton, Tooltip } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import clsx from 'clsx';
import { Icon, InlineIcon } from "@iconify/react";
import RestartIcon from '@iconify/icons-mdi/replay';
import HopIcon from '@iconify/icons-mdi/debug-step-into';
import SkipIcon from '@iconify/icons-mdi/debug-step-over';
import ResumeIcon from '@iconify/icons-mdi/play';
import TerminateIcon from '@iconify/icons-mdi/stop';
import { AppContext } from '../../AppContext';
import CustomList from '../controls/CustomList';
import CodeEditor from '../parts/CodeEditor';
import Inspector from './Inspector';

class Debugger extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            frames: [],
            selectedFrame: null,
            selectedMode: "source",
        }
    }

    componentDidMount() {
        this.updateFrames()
    }

    close = () => {
        this.props.onClose(this.props.id);
    }

    async updateFrames() {
        const frames = await this.context.api.getFrames(this.props.id);
        let selected = null;
        if (frames.length > 0) {
            selected = frames[0];
            await this.updateFrame(selected)
        }
        this.setState({frames: frames, selectedFrame: selected});
    }

    frameSelected = async (frame) => {
        await this.updateFrame(frame);
        this.setState({selectedFrame: frame, selectedMode: "source"});
    }

    updateFrame = async (frame) => {
        if (!frame.method) {
            const info = await this.context.api.getFrame(this.props.id, frame.index);
            frame.method = info.method;
            frame.class = info.class;
            frame.interval = info.interval;
        }
    }

    currentSource = () => {
        const {selectedFrame, selectedMode} = this.state;
        if (!selectedFrame) {return ''}
        let source;
        switch (selectedMode) {
            case "comment":
                source = !selectedFrame.class? "can't access class" : selectedFrame.class.comment;
                break;
            case "definition":
                source = !selectedFrame.class? "can't access class" : selectedFrame.class.definition;
                break;
            case "source":    
                source = !selectedFrame.method? "can't access source" : selectedFrame.method.source;
                break;
            default:
        }
        return source
    }

    currentSelection = () => {
        const {selectedFrame, selectedMode} = this.state;
        if (!selectedFrame || selectedMode !== "source") {return []}
        return [selectedFrame.interval];
    }

    modeChanged = (event, mode) => {
        this.setState({selectedMode: mode})
    }

    saveClicked = (source) => {
        switch (this.state.selectedMode) {
            case "comment":
                this.commentClass(source);
                break;
            case "definition":
                this.defineClass(source);
                break;
            case "source":    
                this.compileMethod(source);
                break;
            default:
        }
    }

    hopClicked = async () => {
        await this.context.api.hop(this.props.id, this.state.selectedFrame.index);
        this.updateFrames();
    }


    skipClicked = async () => {
        await this.context.api.skip(this.props.id, this.state.selectedFrame.index);
        this.updateFrames();
    }

    restartClicked = async () => {
        await this.context.api.restart(this.props.id, this.state.selectedFrame.index);
        this.updateFrames();
    }

    resumeClicked = async () => {
        await this.context.api.resume(this.props.id);
        this.context.closeDebugger(this.props.id);
    }

    terminateClicked = async () => {
        await this.context.api.terminate(this.props.id);
        this.context.closeDebugger(this.props.id);
    }

    render() {
        const {frames, selectedFrame, selectedMode} = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
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
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={8} lg={8}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    itemLabel="label"
                                    selectedItem ={selectedFrame}
                                    items={frames}
                                    onSelect={this.frameSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={4} lg={4}>
                            <Inspector
                                root={null}
                                classes={this.props.classes}/>
                        </Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={12} lg={12}>
                            <ToggleButtonGroup
                                label="primary"
                                value={selectedMode}
                                exclusive
                                onChange={this.modeChanged}>
                                <ToggleButton value="source" variant="outlined" size="small">
                                    Method defintion
                                </ToggleButton>
                                <ToggleButton value="definition" variant="outlined" size="small">
                                    Class definition
                                </ToggleButton>
                                <ToggleButton value="comment" variant="outlined" size="small">
                                    Class comment
                                </ToggleButton>
                            </ToggleButtonGroup>    
                        </Grid>
                        <Grid item xs={12} md={12} lg={12}>
                            <CodeEditor
                                classes={this.props.classes}
                                source={this.currentSource()}
                                selectedRanges={this.currentSelection()}
                                showAccept={true}
                                onAccept={this.saveClicked}
                                />
                        </Grid>
                    </Grid>
                </Grid>
            </Grid>
        )
    }
}

export default Debugger;