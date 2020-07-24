import React, { Component } from 'react';
import {
    Button,
    Dialog,
    DialogActions,
    DialogContent,
    DialogTitle } from '@material-ui/core';

class ConfirmDialog extends Component {
  constructor(props) {
    super();
    this.state = {
      open: false,
    }
  }

  static getDerivedStateFromProps(props, state) {
    if (props.open !== state.open) {
        return {
            open: props.open,
        };
    }
    return null
  }

  yesClicked = () => {
    this.setState({open: false});
    this.props.answer(true);
  }

  noClicked = () => {
    this.setState({open: false});
    this.props.answer(false);
  }

  render() {
    return (
      <Dialog
        open={this.state.open}
        onClose={this.noClicked}>
        <DialogTitle id="confirm-dialog">{this.props.title}</DialogTitle>
        <DialogContent>{this.props.question}</DialogContent>
        <DialogActions>
          <Button
            variant="contained"
            onClick={this.noClicked}
            color="secondary">
            No
          </Button>
          <Button
            variant="contained"
            onClick={this.yesClicked}
            color="default">
            Yes
          </Button>
        </DialogActions>
      </Dialog>
    )
  }
}

export default ConfirmDialog;