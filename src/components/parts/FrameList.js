import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { AppContext } from '../../AppContext';

class FrameList extends Component {
    static contextType = AppContext;

    frameSelected = (frame) => {
        const handler = this.props.onSelect;
        if (handler) {handler(frame)}
    }

    menuOptions() {
        return [
            {label: 'Browse', action: f => this.context.browseClass(f.class.name)},
            {label: 'Senders', action: f => this.context.browseSenders(f.method.selector)},
            {label: 'Local senders', action: f => this.context.browseLocalSenders(f.method.selector, f.class.name)},
            {label: 'Implementors', action: f => this.context.browseImplementors(f.method.selector)},
            {label: 'Local implementors', action: f => this.context.browseLocalImplementors(f.method.selector, f.class.name)},
            {label: 'Class references', action: f => this.context.browseReferences(f.class.name)},
        ]
    }

    render() {
        return (
            <CustomList
                itemLabel="label"
                items={this.props.frames}
                selectedItem={this.props.selectedFrame}
                onSelect={this.frameSelected}
                menuOptions={this.menuOptions()}
            />
        )
    }
};

export default FrameList;