import React from 'react';
import {
    Button,
    Dialog,
    DialogActions,
    DialogContent,
    DialogTitle }
 from '@material-ui/core';

const ConfirmDialog = (props) => {
  const {title, children, open, setOpen, onConfirm} = props;
  return (
    <Dialog
      open={open}
      onClose={() => setOpen(false)}
    >
      <DialogTitle id="confirm-dialog">{title}</DialogTitle>
      <DialogContent>{children}</DialogContent>
      <DialogActions>
        <Button
          variant="contained"
          onClick={() => setOpen(false)}
          color="secondary"
        >
          No
        </Button>
        <Button
          variant="contained"
          onClick={() => {
            setOpen(false);
            onConfirm();
          }}
          color="default"
        >
          Yes
        </Button>
      </DialogActions>
    </Dialog>
  );
};
export default ConfirmDialog;