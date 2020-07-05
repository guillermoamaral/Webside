import React, { Component } from 'react';
import { Paper } from '@material-ui/core';

class Transcript extends Component {
    constructor(props) {
        super(props);
        this.state = {
            text: 'This is the transcript'
        }
    }

    render() {
        return (
            <Paper>
                <p>{this.state.text}</p>      
            </Paper>
        )
    }
}

export default Transcript;
