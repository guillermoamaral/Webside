import React, { Component } from 'react';
import SimpleList from './SimpleList';
import { ArrowUpDownBold, ArrowUpBold, ArrowDownBold } from 'mdi-material-ui';

class SelectorList extends Component {
    render() {
        const size = 14;
        const selectors = this.props.selectors == null ? [] : this.props.selectors;
        return (
            <SimpleList
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
                    return null})}
                onSelect={this.props.onSelect}/>
        )
    }
};

export default SelectorList;