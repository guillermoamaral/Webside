import React, { Component } from 'react';
import SimpleList from './SimpleList';

class SelectorList extends Component {
    render() {
        return (
            <SimpleList
                items={this.props.selectors}
                onSelect={this.props.onSelect}/>
        )
    }
};

export default SelectorList;