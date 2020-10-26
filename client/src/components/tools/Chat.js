import React, { Component } from 'react';
import socketio from 'socket.io-client';
import { Grid, Paper, Box, TextField, IconButton } from '@material-ui/core';
import clsx from 'clsx';
import CustomList from '../controls/CustomList';
import SendIcon from '@material-ui/icons/Send';

class Chat extends Component {
    constructor(props) {
        super(props);
        this.io = socketio('http://localhost:4200');
        this.state = {
            message: '',
            messages: []
          }
    }

    componentDidMount() {
        this.io.on('NEW_MESSAGE_RECEIVED', payload => {
            this.setState({
            messages: [
                ...this.state.messages,
                payload
            ]
            })
        })
    }

    sendMessage = () => {
        this.setState({message: ''})
        this.io.emit('NEW_MESSAGE', {
            username: this.props.username,
            message: this.state.message
        })
    }

    render() {
        const messages = this.state.messages;
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={0}>
                <Grid item xs={12} md={12} lg={12}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <CustomList
                            itemLabel={m => m.username + ": " + m.message}
                            items={messages}/>
                    </Paper>
                </Grid>
                <Grid item xs={11} md={11} lg={11}>
                    <TextField
                        value={this.state.message}
                        onChange={event => this.setState({message: event.target.value})}
                        placeholder="Send a message ...."
                        name="message"
                        variant="outlined"
                        fullWidth
                        margin="dense"
                        autoFocus
                        type="text"/>
                </Grid>
                <Grid item xs={1} md={1} lg={1}>
                    <Box display="flex" justifyContent="center" > 
                        <IconButton onClick={this.sendMessage}>
                            <SendIcon size="small"/>
                        </IconButton>
                    </Box>
                </Grid>
            </Grid>
        )
    }
}

export default Chat;
