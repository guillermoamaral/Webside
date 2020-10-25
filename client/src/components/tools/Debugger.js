import React, { PureComponent } from 'react';
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
import FrameList from '../parts/FrameList';
import CodeBrowser from '../parts/CodeBrowser';
import CodeEditor from '../parts/CodeEditor';

class Debugger extends PureComponent {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            frames: [],
            selectedFrame: null,
            selectedBinding: null,
        }
    }

    componentDidMount() {
        this.updateFrames()
    }

    async updateFrames() {
        const frames = await this.context.api.getDebuggerFrames(this.props.id);
        let selected = null;
        if (frames.length > 0) {
            selected = frames[0];
            await this.updateFrame(selected)
        }
        this.setState({
            frames: frames,
            selectedFrame: selected,
            selectedBinding: selected.bindings.find(b => b.name = 'self')
        });
    }

    frameSelected = async (frame) => {
        await this.updateFrame(frame);
        this.setState({selectedFrame: frame, selectedBinding: frame.bindings.find(b => b.name = 'self')});
    }

    bindingSelected = async (binding) => {
        this.setState({selectedBinding: binding});
    }

    updateFrame = async (frame) => {
        if (!frame.method) {
            const info = await this.context.api.getDebuggerFrame(this.props.id, frame.index);
            frame.method = info.method;
            frame.class = info.class;
            frame.interval = info.interval;
        }
        if (!frame.bindings) {
            const bindings = await this.context.api.getFrameBindings(this.props.id, frame.index);
            frame.bindings = bindings;
        }
    }

    hopClicked = async () => {
        await this.context.api.hopDebugger(this.props.id, this.state.selectedFrame.index);
        this.updateFrames();
    }

    skipClicked = async () => {
        await this.context.api.skipDebugger(this.props.id, this.state.selectedFrame.index);
        this.updateFrames();
    }

    restartClicked = async () => {
        await this.context.api.restartDebugger(this.props.id, this.state.selectedFrame.index);
        this.updateFrames();
    }

    resumeClicked = async () => {
        await this.context.api.resumeDebugger(this.props.id);
        this.context.closeDebugger(this.props.id);
    }

    terminateClicked = async () => {
        await this.context.api.terminateDebugger(this.props.id);
        this.context.closeDebugger(this.props.id);
    }

    methodCompiled = async (method) => {
        const selected = this.state.selectedFrame.method;
        if (method.selector === selected.selector) {
            await this.context.api.restartDebugger(this.props.id, this.state.selectedFrame.index, true);;
            this.updateFrames();
        }
    }

    render() {
        const {frames, selectedFrame, selectedBinding} = this.state;
        const styles = this.props.styles;
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
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={8} lg={8}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <FrameList
                                    frames={frames}
                                    selectedFrame ={selectedFrame}
                                    onSelect={this.frameSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={2} lg={2}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    itemLabel="name"
                                    selectedItem ={selectedBinding}
                                    items={selectedFrame? selectedFrame.bindings : []}
                                    onSelect={this.bindingSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={2} lg={2}>
                            <CodeEditor
                                styles={this.props.styles}
                                lineNumbers={false}
                                source={selectedBinding? selectedBinding.value : ''}
                                onAccept={this.saveBinding}/>
                        </Grid>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <CodeBrowser
                        context={{debugger: this.props.id, frame: selectedFrame? selectedFrame.index : null}}
                        styles={styles}
                        class={selectedFrame? selectedFrame.class : null}
                        method={selectedFrame? selectedFrame.method : null}
                        selectedInterval={selectedFrame? selectedFrame.interval : null}
                        onMethodCompiled={this.methodCompiled}
                        onClassDefined={this.classDefined}
                        onClassCommented={this.classCommented}/>
                </Grid>
            </Grid>
        )
    }
}

export default Debugger;