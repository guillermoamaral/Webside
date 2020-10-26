import React, { Component } from 'react';
import socketio from 'socket.io-client';
import { Grid, Paper, Box, TextField, IconButton } from '@material-ui/core';
import clsx from 'clsx';
import CustomList from '../controls/CustomList';
import SendIcon from '@material-ui/icons/Send';

class Chat extends Component {
    constructor(props) {
        super(props);
        this.io = socketio(this.props.url);
        this.state = {
            id: null,
            contacts: this.props.contacts,
            selectedContact: null,
            messages: [],
            text: '',
          }
    }

    componentDidMount() {
        this.io.emit('login', {username: this.props.username});
        this.io.on('logged', data => this.setState({id: data.id}));
        this.io.on('users', users => {
            const contacts = users.filter(u => u.username !== this.props.username);
            contacts.push({username: '<all>'})
            this.setState({contacts: contacts})});
        this.io.on('receive', message => this.setState({messages: [...this.state.messages, message]}));
    }

    sendMessage = () => {
        const contact = this.state.selectedContact;
        const to = !contact || contact.username === '<all>'? null : contact; 
        const message = {
            date: new Date(Date.now()).toUTCString(),
            from: {id: this.state.id, username: this.props.username},
            to: to,
            text: this.state.text
        };
        if (to) {
            this.setState({text: '', messages: [...this.state.messages, message]});
        } else {
            this.setState({text: ''});
        }
        this.io.emit('send', message);
    }

    contactSelected = (contact) => {
        this.setState({selectedContact: contact})
    }

    render() {
        const {contacts, selectedContact, messages, text} = this.state;
        const filtered = messages.filter(m => 
            selectedContact && (
                (m.from.username === selectedContact.username) ||
                (m.to && m.to.username === selectedContact.username) ||
                (!m.to && selectedContact.username === '<all>')
            )
        );
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={4} md={4} lg={4}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <CustomList
                            itemLabel="username"
                            items={contacts}
                            selectedItem={selectedContact}
                            onSelect={this.contactSelected}/>
                    </Paper>                    
                </Grid>
                <Grid item xs={8} md={8} lg={8}>
                    <Grid container spacing={1}>
                        <Grid item xs={12} md={12} lg={12}>
                            <Paper className={fixedHeightPaper} variant="outlined">
                                <CustomList
                                    itemLabel={m => m.from.username + " (" + new Date(m.date).toLocaleTimeString() + "): " + m.text}
                                    items={filtered}/>
                            </Paper>
                        </Grid>
                        <Grid item xs={11} md={11} lg={11}>
                            <TextField
                                value={text}
                                onChange={event => this.setState({text: event.target.value})}
                                placeholder="Send a message ...."
                                name="text"
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
                </Grid>
            </Grid>
        )
    }
}

export default Chat;
