import React, { Component } from 'react';
import CustomList from './CustomList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';

class SelectorList extends Component {
    removeSelector = (selector) => {
        this.props.api.removeMethod(selector.class, selector.selector)
            .then(response => {
                if (this.props.onRemoved !== undefined) {
                    this.props.onRemoved(selector)
                }
            })
            .catch(error => {})
    }

    render() {
        const size = 14;
        const selectors = this.props.selectors == null ? [] : this.props.selectors;
        return (
            <CustomList
                label={this.props.showClass === true ? ((s) => {return s.class + '>>#' + s.selector}) : "selector"}
                items={selectors}
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
                    {label: 'Rename', action: this.renameSelector},
                    {label: 'Remove', action: this.removeSelector},
                    {label: 'Senders', action: this.browseSenders},
                    {label: 'Implementors', action: this.browseImplementors},]}
                onSelect={this.props.onSelect}/>
        )
    }
};

export default SelectorList;