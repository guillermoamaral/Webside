import React, { Component } from 'react';
import CustomList from './CustomList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';
import { AppContext } from '../AppContext';

class MethodList extends Component {
    static contextType = AppContext;

    removeMethod = (method) => {
        this.context.api.deleteMethod(method.class, method.selector)
            .then(response => {
                const handler = this.props.onRemoved;
                if (handler !== undefined) {
                    handler(method)
                }
            })
            .catch(error => {})
    }

    menuOptions() {
        const local = 
            [
                {label: 'Rename', action: this.renameMethod},
                {label: 'Remove', action: this.removeMethod},
                null,
                {label: 'Senders', action: m => this.context.browseSenders(m.selector)},
                {label: 'Implementors', action: m => this.context.browseImplementors(m.selector)},
                {label: 'Class references', action: m => this.context.browseReferences(m.selector)}
            ];
        const external = this.props.menuOptions; 
        if (external === undefined) {return local}
        return external.concat(local);
    }

    render() {
        const size = 12;
        const methods = this.props.methods == null ? [] : this.props.methods;
        return (
            <CustomList
                itemLabel={this.props.showClass === true ? (m => {return m.class + '>>#' + m.selector}) : "selector"}
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

export default MethodList;