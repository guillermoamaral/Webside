import React, { Component } from 'react';
import {
    Button,
    Dialog,
    DialogActions,
    DialogContent,
    DialogTitle } from '@material-ui/core';

class Prompter extends Component {
    constructor(props) {
        super();
        this.state = {
            open: false,
            data: '',
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.open !== state.open) {
            return {open: props.open}
        }
        return null;
    }

    okClicked = () => {
        this.setState({open: false});
        this.props.answer(this.state.data);
    }

    cancelClicked = () => {
        this.setState({open: false});
        this.props.answer(null);
    }

    render() {
        return (
            <Dialog
                open={this.state.open}
                onClose={this.cancelClicked}>
                <DialogTitle id="confirm-dialog">{this.props.title}</DialogTitle>
                <DialogContent>
                    {this.props.question}
                </DialogContent>
                <DialogActions>
                    <Button
                        variant="contained"
                        onClick={this.cancelClicked}
                        color="secondary">
                            Cancel
                    </Button>
                    <Button
                        variant="contained"
                        onClick={this.okClicked}
                        color="default">
                            Ok
                        </Button>
                </DialogActions>
            </Dialog>
        )
    }
}

export default Prompter;