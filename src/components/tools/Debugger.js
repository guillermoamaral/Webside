import React, { Component } from 'react';
import { Grid, Paper } from '@material-ui/core';
import { ToggleButton , ToggleButtonGroup } from '@material-ui/lab';
import clsx from 'clsx';
import { AppContext } from '../../AppContext';
import CustomList from '../controls/CustomList';
import CodeEditor from '../parts/CodeEditor';

class Debugger extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            frames: [],
            selectedFrame: null,
            selectedMode: null,
        }
    }

    componentDidMount() {
        this.updateFrames()
    }

    close = () => {
        this.props.onClose(this.props.id);
    }

    updateFrames() {
        this.context.api.getFrames(this.props.id)
            .then(frames => {this.setState({frames: frames})})
            .catch(error => {})
    }

    frameSelected = async (frame) => {
        await this.updateMethod(frame);
        await this.updateClass(frame);
        this.setState({selectedFrame: frame, selectedMode: "source"});
    }

    updateMethod = async (frame) => {
        if (frame.method === undefined) {
            const info = await this.context.api.getFrame(this.props.id, frame.index);
            frame.method = info.method;
        }
    }

    updateClass = async (frame) => {
        if (frame.class === undefined && frame.method !== null) {
            const definition = await this.context.api.getClass(frame.method.class);
            frame.class = definition;
        }
    }

    currentSource = () => {
        const {selectedFrame, selectedMode} = this.state;
        if (selectedFrame === null) {return ''}
        let source;
        switch (selectedMode) {
            case "comment":
                source = selectedFrame.class === null? "can't access class" : selectedFrame.class.comment;
                break;
            case "definition":
                source = selectedFrame.class === null? "can't access class" : selectedFrame.class.definition;
                break;
            case "source":    
                source = selectedFrame.method === null? "can't access source" : selectedFrame.method.source;
                break;
            default:
        }
        return source
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

    render() {
        const {frames, selectedFrame, selectedMode} = this.state;
        const fixedHeightPaper = clsx(this.props.classes.paper, this.props.classes.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={12} lg={12}>
                    Controls
                </Grid>
                <Grid item xs={12} md={12} lg={12}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={6} lg={6}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    itemLabel="label"
                                    selectedItem ={selectedFrame}
                                    items={frames}
                                    onSelect={this.frameSelected}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            Variables
                        </Grid>
                        <Grid item xs={12} md={3} lg={3}>
                            Inspector
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