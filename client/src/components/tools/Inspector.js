import React, { Component } from 'react';
import { Box, Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';
import { IDEContext } from '../../IDEContext';
import CustomTree from '../controls/CustomTree';
import CodeEditor from '../parts/CodeEditor';

class Inspector extends Component {
    static contextType = IDEContext;

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
            const dictionary = await this.context.evaluateExpression('self isKindOf: Dictionary', false, {object: this.props.id});
            if (dictionary.printString === 'true') {
                const keys = await this.context.evaluateExpression('self keys', false, {object: this.props.id});
                console.log(keys);
                slots = await this.context.api.getInstanceVariables(object.class);
                console.log(slots)
            } else {
                slots = await this.context.api.getInstanceVariables(object.class);
                slots = slots.map(s => s.name)
            }
        }
        if (slots.length === 0) {return}
        object.slots = [];
        slots.forEach(async s => {
            const path = object.path + '/' + s;
            const slot = await this.context.api.getSlot(this.props.root.id, path);
            slot.name = s.toString();
            slot.path = path;
            object.slots.push(slot);
            this.setState({objectTree: this.state.objectTree});
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
        const {styles, showWorkspace} = this.props;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={6} lg={6}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <CustomTree
                            items={objectTree}
                            itemLabel="name"
                            id="path"
                            children={"slots"}
                            selectedItem={this.selectedObject}
                            onExpand={this.slotExpanded}
                            onSelect={this.slotSelected}/>
                    </Paper>
                </Grid>
                <Grid item xs={12} md={6} lg={6}>
                    <CodeEditor
                        context={{object: this.props.id}}
                        styles={styles}
                        lineNumbers={false}
                        source={!selectedObject? "" : selectedObject.printString}
                        onChange={this.props.onChange}
                        onAccept={this.props.onAccept}/>
                </Grid>
                {showWorkspace && <Grid item xs={12} md={12} lg={12}>
                     <CodeEditor
                        context={{object: this.props.id}}
                        styles={styles}
                        lineNumbers={false}/>
                </Grid>}
            </Grid>
        )
    };
}

export default Inspector;