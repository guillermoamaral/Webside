import React, { Component } from 'react';
import { Box, Grid, Paper } from '@material-ui/core';
import clsx from 'clsx';
import { IDEContext } from '../IDEContext';
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
        if (!object) {return}
        if (object.slots) {return}
        const slots = [];
        try {
            if (object.indexable) {
                for(var i = 0; i < object.size; i++) {slots.push(i + 1)}
            } else {
                const dictionary = await this.context.evaluateExpression('self isKindOf: Dictionary', true, false, {object: this.props.id});
                if (dictionary.printString === 'true_') {return this.updateDictionarySlots(object)}
                const vars = await this.context.api.getInstanceVariables(object.class);
                vars.forEach(v => slots.push(v.name));
            }
            object.slots = [];
            if (slots.length === 0) {return}
            slots.forEach(async s => {
                const path = object.path + '/' + s;
                const slot = await this.context.api.getSlot(this.props.root.id, path);
                slot.name = s.toString();
                slot.path = path;
                object.slots.push(slot);
                this.setState({objectTree: this.state.objectTree});
            })
        }
        catch (error) {this.context.reportError(error)}
    }

    updateDictionarySlots = async (object) => {
        try {
            const keys = await this.context.evaluateExpression('self keys asArray', true, true, {object: this.props.id});
            const values = await this.context.evaluateExpression('self values asArray', true, true, {object: this.props.id});
            object.slots = [];
            for(var i = 1; i <= keys.size; i++) {
                const key = await this.context.api.getSlot(keys.id, '/' + i);
                const path = values.id + '/' + i;
                const value = await this.context.api.getSlot(path);
                value.name = key.printString;
                value.path = path;
                object.slots.push(value);
                this.setState({objectTree: this.state.objectTree});
            }
            await this.context.api.unpinObject(keys.id);
        }
        catch (error) {this.context.reportError(error)}
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