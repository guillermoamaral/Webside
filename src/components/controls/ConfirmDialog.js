import { React, Component } from 'react';
import {
    Button,
    Dialog,
    DialogActions,
    DialogContent,
    DialogTitle }
 from '@material-ui/core';

class ConfirmDialog extends Component {
  yesClicked = () => {
    const handler = this.props.onConfirm;
    if (handler !== undefined) {handler()}
  }

  noClicked = () => {
    const handler = this.props.onCancel;
    if (handler !== undefined) {handler()}
  }

  render() {
    const {title, question, open} = this.props;
    return (
      <Dialog
        open={open}
        onClose={this.noClicked}
      >
        <DialogTitle id="confirm-dialog">{title}</DialogTitle>
        <DialogContent>{question}</DialogContent>
        <DialogActions>
          <Button
            variant="contained"
            onClick={this.noClicked}
            color="secondary"
          >
            No
          </Button>
          <Button
            variant="contained"
            onClick={this.yesClicked}
            color="default"
          >
            Yes
          </Button>
        </DialogActions>
      </Dialog>
    )
  }
}

export default ConfirmDialog; 