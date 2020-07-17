import React, { Component } from 'react';
import CustomList from './CustomList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';

class MethodList extends Component {
    removeMethod = (method) => {
        this.props.api.deleteMethod(method.class, method.selector)
            .then(response => {
                const handler = this.props.onRemoved;
                if (handler !== undefined) {
                    handler(method)
                }
            })
            .catch(error => {})
    }

    browseSenders = (method) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseSenders;
        if (option !== undefined) {
            option(method.selector)
        }
    }

    browseImplementors = (method) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseImplementors;
        if (option !== undefined) {
            option(method.selector)
        }
    }

    browseReferences = (method) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseReferences;
        if (option !== undefined) {
            option(method.class)
        }
    }

    render() {
        const size = 12;
        const methods = this.props.methods == null ? [] : this.props.methods;
        return (
            <CustomList
                label={this.props.showClass === true ? (m => {return m.class + '>>#' + m.selector}) : "selector"}
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
                menuOptions={[
                    {label: 'New', action: this.newMethod},
                    {label: 'Rename', action: this.renameMethod},
                    {label: 'Remove', action: this.removeMethod},
                    null,
                    {label: 'Senders', action: this.browseSenders},
                    {label: 'Implementors', action: this.browseImplementors},
                    {label: 'Class references', action: this.browseReferences}]}
            />
        )
    }
};

export default MethodList;