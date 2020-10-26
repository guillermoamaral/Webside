import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { AppContext } from '../../AppContext';

class FrameList extends Component {
    static contextType = AppContext;

    frameSelected = (frame) => {
        const handler = this.props.onSelect;
        if (handler) {handler(frame)}
    }

    browseClass = (frame) => {
        if (frame) {this.context.browseClass(frame.class.name)}
    }

    browseSenders = (frame) => {
        if (frame) {this.context.browseSenders(frame.method.selector)}
    }

    browseSenders = (frame) => {
        if (frame) {this.context.browseLocalSenders(frame.method.selector, frame.class.name)}
    }

    browseImplementors = (frame) => {
        if (frame) {this.context.browseImplementors(frame.method.selector)}
    }

    browseLocalImplementors = (frame) => {
        if (frame) {this.context.browseLocalImplementors(frame.method.selector, frame.class.name)}
    }

    browseReferences = (frame) => {
        if (frame) {this.context.browseReferences(frame.class.name)}
    }

    menuOptions() {
        return [
            {label: 'Browse', action: this.browseClass},
            {label: 'Senders', action: this.browseSenders},
            {label: 'Local senders', action: this.browseSenders},
            {label: 'Implementors', action: this.browseImplementors},
            {label: 'Local implementors', action: this.browseLocalImplementors},
            {label: 'Class references', action: this.browseReferences},
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