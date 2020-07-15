import React, { Component } from 'react';
import CustomList from './CustomList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';

class SelectorList extends Component {
    removeSelector = (selector) => {
        this.props.api.deleteMethod(selector.class, selector.selector)
            .then(response => {
                if (this.props.onRemoved !== undefined) {
                    this.props.onRemoved(selector)
                }
            })
            .catch(error => {})
    }

    browseSenders = (selector) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseSenders;
        if (option !== undefined) {
            option(selector.selector)
        }
    }

    browseImplementors = (selector) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseImplementors;
        if (option !== undefined) {
            option(selector.selector)
        }
    }

    browseReferences = (selector) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseReferences;
        if (option !== undefined) {
            option(selector.class)
        }
    }

    render() {
        const size = 12;
        const selectors = this.props.selectors == null ? [] : this.props.selectors;
        return (
            <CustomList
                label={this.props.showClass === true ? ((s) => {return s.class + '>>#' + s.selector}) : "selector"}
                items={selectors}
                selectedItem={this.props.selectedSelector}
                onSelect={this.props.onSelect}
                icons={selectors.map(s => {
                    if (s.overriding && s.overriden) {
                        return <ArrowUpDownBold style={{fontSize: size}} />
                    }
                    if (s.overrriding) {
                        return <ArrowUpBold style={{fontSize: size}} />
                    }
                    if (s.overriden) {
                        return <ArrowDownBold style={{fontSize: size}} />    
                    }
                    return null
                })}
                menuOptions={[
                    {label: 'New', action: this.newSelector},
                    {label: 'Rename', action: this.renameSelector},
                    {label: 'Remove', action: this.removeSelector},
                    null,
                    {label: 'Senders', action: this.browseSenders},
                    {label: 'Implementors', action: this.browseImplementors},
                    {label: 'Class references', action: this.browseReferences}]}
            />
        )
    }
};

export default SelectorList;