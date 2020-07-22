import React, { Component } from 'react';
import { Grid } from '@material-ui/core';
import CodeEditor from '../parts/CodeEditor';

class Transcript extends Component {
    constructor(props) {
        super(props);
        this.state = {
            text: props.text,
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.text !== state.text) {
            return {
                text: props.text
            };
        }
        return null
    }

    render() {
        return (
            <Grid container>
                <Grid item xs={12} md={12} lg={12}>
                    <CodeEditor
                        classes={this.props.classes}
                        source={this.props.text}
                        onAccept={this.saveClicked}/>
                </Grid>
            </Grid>
        )
    }
}

export default Transcript;
