import React, { Component } from 'react';
import { Box, Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';
import { AppContext } from '../../AppContext';
import CustomTree from '../controls/CustomTree';
import CodeEditor from '../parts/CodeEditor';

class Inspector extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        const root = this.props.root;
        if (root) {
            root.name = 'self';
            root.path = '';    
        }
        this.state = {
            root: root,
            objectTree: !root? [] : [root],
            objects: {},
            selectedObject: !root? null : root,
        }
    }

    componentDidMount() {
        this.updateSlots(this.state.root)
    }

    updateSlots = async (object) => {
        if (!object) {return []}
        if (object.slots) {return object.slots}
        let slots;
        if (object.indexable) {
            slots = [];
            for(var i = 0; i < object.size; i++) {slots.push(i + 1)}
        } else {
            slots = await this.context.api.getInstanceVariables(object.class);
            slots = slots.map(s => s.name)
        }
        object.slots = [];
        slots.forEach(async s => {
            const path = object.path + '/' + s;
            const slot = await this.context.api.getSlot(this.props.root.id, path);
            slot.name = s;
            slot.path = path;
            object.slots.push(slot);
            const objects = this.state.objects;
            objects[slot.id] = slot;
            this.setState({objectTree: this.state.objectTree, objects: objects});
        })
    }

    slotSelected = (object) => {
        this.setState({selectedObject: object})
    }

    slotExpanded = (object) => {
        if (object) {object.slots.forEach(s => this.updateSlots(s))}
    }

    render() {
        const {objectTree, selectedObject} = this.state;
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Box className={styles.box}>
                <Grid container spacing={1}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={6} lg={6}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomTree
                                    items={objectTree}
                                    itemLabel="name"
                                    id="id"
                                    children={"slots"}
                                    selectedItem={this.selectedObject}
                                    onExpand={this.slotExpanded}
                                    onSelect={this.slotSelected}
                                />
                            </Paper>
                        </Grid>
                        <Grid item xs={12} md={6} lg={6}>
                            <CodeEditor
                                styles={styles}
                                lineNumbers={false}
                                source={!selectedObject? "" : selectedObject.printString}
                                onChange={this.props.onChange}
                                onAccept={this.props.onAccept}/>
                        </Grid>
                    </Grid>
                </Grid>
            </Box>
        )
    };
}

export default Inspector;