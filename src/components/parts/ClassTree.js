import React, { Component } from 'react';
import CustomTree from '../controls/CustomTree';
import { AppContext } from '../../AppContext';
import { withDialog } from '../dialogs';
import Scrollable from '../controls/Scrollable';

class ClassTree extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {root: props.root}
    }

    // static getDerivedStateFromProps(props, state) {
    //     if (props.root !== state.root) {
    //         return {
    //             items: props.items,
    //         };
    //     }
    //     return null
    // }

    createClass = async (superclass) => {
        const name = await this.props.dialog.prompt({title: 'Create subclass'});
        if (!name) {return}
        const definition = superclass.name + ' subclass: #' + name + 
            ' instanceVariableNames: \'\' classVariableNames: \'\' poolDictionaries: \'\'';
        const species = await this.context.api.defineClass(name, definition);
        const handler = this.props.onCreate; 
        if (handler) {handler(species)}
    }

    removeClass = async (species) => {
        const confirm = await this.props.dialog.confirm({
            title: 'Delete ' + species.name + '?',
            ok: {text: 'Delete', color: "secondary", variant: "outlined"}});
        if (!confirm) {return}
        await this.context.api.deleteClass(species.name);
        const handler = this.props.onRemove; 
        if (handler) {handler(species)}
    }

    renameClass = async (species) => {
        if (!species) {return}
        try {
            const newName = await this.props.dialog.prompt({title: 'Rename class', defaultValue: species.name});
            await this.context.api.renameClass(species.name, newName);
            species.name = newName;
            const handler = this.props.onRename; 
            if (handler) {handler(species)}
        }
        catch (error) {}
    }

    menuOptions() {
        return [
            {label: 'New', action: this.createClass},
            {label: 'Rename', action: this.renameClass},
            {label: 'Remove', action: this.removeClass},
            null,
            {label: 'Browse', action: c => this.context.browseClass(c.name)},
            {label: 'References', action: c => this.context.browseReferences(c.name)},
            null,
            {label: 'Test', action: c => this.context.runTestClass(c.name)},
        ]
    }

    render() {
        const root = this.props.root;
        return (
            <CustomTree
                items={root? [root] : []}
                itemLabel="name"
                children={"subclasses"}
                onExpand={this.props.onExpand}
                onSelect={this.props.onSelect}
                selectedItem={this.props.selectedClass}
                menuOptions={this.menuOptions()}/>
        )
    }
}
export default withDialog()(ClassTree);