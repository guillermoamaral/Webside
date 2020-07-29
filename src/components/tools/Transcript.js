import React, { Component } from 'react';
import CodeEditor from '../parts/CodeEditor';

class Transcript extends Component {
    render() {
        return (
            <CodeEditor
                classes={this.props.classes}
                source={this.props.text}
                onChange={this.props.onChange}
                onAccept={this.props.onAccept}/>
        )
    }
}

export default Transcript;
