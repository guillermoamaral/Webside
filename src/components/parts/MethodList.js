import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';
import { AppContext } from '../../AppContext';
import { withDialog } from '../dialogs';
import { Grid } from '@material-ui/core';
import Pagination from '@material-ui/lab/Pagination';

class MethodList extends Component {
    static contextType = AppContext;

    newMethod = () => {
        const method = {
            class: this.props.selectedMethod? this.props.selectedMethod.class : null,
            category: this.props.selectedMethod? this.props.selectedMethod.category : null,
            source: 'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements'
        }
        this.props.onSelect(method)
    }

    renameMethod = async (method) => {
        if (!method) {return}
        try{
            const newSelector = await this.props.dialog.prompt({title: 'Rename selector', defaultValue: method.selector});
            await this.context.api.renameSelector(method.class, method.selector, newSelector);
            method.selector = newSelector;
            const handler = this.props.onRename;
            if (handler) {handler(method)}
        }
        catch (error) {}
    }

    removeMethod = async (method) => {
        await this.context.api.deleteMethod(method.class, method.selector);
        const handler = this.props.onRemove;
        if (handler) {handler(method)}
    }

    menuOptions() {
        return [
            {label: 'New', action: this.newMethod},
            {label: 'Rename', action: this.renameMethod},
            {label: 'Remove', action: this.removeMethod},
            null,
            {label: 'Browse', action: m => this.context.browseClass(m.class)},
            {label: 'Senders', action: m => this.context.browseSenders(m.selector)},
            {label: 'Local senders', action: m => this.context.browseLocalSenders(m.selector, m.class)},
            {label: 'Implementors', action: m => this.context.browseImplementors(m.selector)},
            {label: 'Local implementors', action: m => this.context.browseLocalImplementors(m.selector, m.class)},
            {label: 'Class references', action: m => this.context.browseReferences(m.class)},
            null,
            {label: 'Test', action: m => this.context.runTest(m.class, m.selector)},
        ]
    }

    render() {
        const usePagination = this.props.usePagination || true;
        const size = 12;
        const methods = !this.props.methods? [] : this.props.methods;
        return (
            <CustomList
                itemLabel={this.props.showClass === true ? (m => m.class + '>>#' + m.selector) : "selector"}
                items={methods}
                selectedItem={this.props.selectedMethod}
                onSelect={this.props.onSelect}
                icons={methods.map(m => {
                    if (m.overriding && m.overriden) {
                        return <ArrowUpDownBold style={{fontSize: size}} />
                    }
                    if (m.overrriding) {
                        return <ArrowUpBold style={{fontSize: size}} />
                    }
                    if (m.overriden) {
                        return <ArrowDownBold style={{fontSize: size}} />    
                    }
                    return null
                })}
                menuOptions={this.menuOptions()}
            />
        )
    }
};

export default withDialog()(MethodList);