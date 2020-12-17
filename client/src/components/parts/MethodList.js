import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';
import { IDEContext } from '../IDEContext';
import { withDialog } from '../dialogs';

class MethodList extends Component {
    static contextType = IDEContext;

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
        catch (error) {this.context.reportError(error)}
    }

    removeMethod = async (method) => {
        try {
            await this.context.api.deleteMethod(method.class, method.selector);
            const handler = this.props.onRemove;
            if (handler) {handler(method)}
        }
        catch (error) {this.context.reportError(error)}
    }   

    browseClass = (method) => {
        if (method) {this.context.browseClass(method.class)}
    }
    
    browseSenders = (method) => {
        if (method) {this.context.browseSenders(method.selector)}
    }
    
    browseLocalSenders = (method) => {
        if (method) {this.context.browseLocalSenders(method.selector, method.class)}
    }
    
    browseImplementors = (method) => {
        if (method) {this.context.browseImplementors(method.selector)}
    }
    
    browseLocalImplementors = (method) => {
        if (method) {this.context.browseLocalImplementors(method.selector, method.class)}
    }
    
    browseReferences = (method) => {
        if (method) {this.context.browseReferences(method.class)}
    }

    runTest = (method) => {
        if (method) {this.context.runTest(method.class, method.selector)}
    }
    
    menuOptions() {
        return [
            {label: 'New', action: this.newMethod},
            {label: 'Rename', action: this.renameMethod},
            {label: 'Remove', action: this.removeMethod},
            null,
            {label: 'Browse', action: this.browseClass},
            {label: 'Senders', action: this.browseSenders},
            {label: 'Local senders', action: this.browseLocalSenders},
            {label: 'Implementors', action: this.browseImplementors},
            {label: 'Local implementors', action: this.browseLocalImplementors},
            {label: 'Class references', action: this.browseReferences},
            null,
            {label: 'Test', action: this.runTest},
        ]
    }

    render() {
        const size = 12;
        const methods = !this.props.methods? [] : this.props.methods;
        const icons = methods.map(m => {
            if (m.overriding && m.overriden) {
                return <ArrowUpDownBold style={{fontSize: size}}/>
            }
            if (m.overrriding) {
                return <ArrowUpBold style={{fontSize: size}}/>
            }
            if (m.overriden) {
                return <ArrowDownBold style={{fontSize: size}}/>    
            }
            return null
        });
        return (
            <CustomList
                itemLabel={this.props.showClass === true ? (m => m.class + '>>#' + m.selector) : "selector"}
                items={methods}
                icons={icons}
                selectedItem={this.props.selectedMethod}
                onSelect={this.props.onSelect}
                menuOptions={this.menuOptions()}/>
        )
    }
}

export default withDialog()(MethodList);