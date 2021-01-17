import React, { Component } from 'react';
import { Grid, Paper, Box, TextField, IconButton } from '@material-ui/core';
import clsx from 'clsx';
import CustomList from '../controls/CustomList';
import SendIcon from '@material-ui/icons/Send';

class Chat extends Component {
    constructor(props) {
        super(props);
        this.client = this.props.client;
        this.state = {
            selectedContact: this.props.initialContact,
            text: '',
          }
    }

    componentDidMount() {
        this.client.onEvent("onContactsUpdated", this.contactsUpdated, this);
        this.client.onEvent("onMessageReceived", this.messageReceived, this);
    }

    componentWillUnmount() {
        this.client.removeHandlers(this);
    }

    contactsUpdated = (contacts) => {
        const selected = this.state.selectedContact? contacts.find(c => c.id === this.state.selectedContact.id) : null;
        this.setState({selectedContact: selected});
    }

    messageReceived = (message) => {
        //this.setState(this.state)
    }

    sendMessage = () => {
        this.client.sendText(this.state.text, this.state.selectedContact)
        this.setState({text: ''});
    }

    contactSelected = (contact) => {
        this.client.markSeenMessagesFrom(contact);
        this.setState({selectedContact: contact})
    }

    contactLabel = (contact) => {
        const unseen = this.client.unseenMessagesFrom(contact);
        let label = contact.username; 
        if (unseen > 0) {label = label + " (" + unseen + ")"};
        return label;
    }

    render() {
        const {selectedContact, text} = this.state;
        const messages = this.client.messagesFrom(selectedContact);
        const styles = this.props.styles;
        const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
        return (
            <Grid container spacing={1}>
                <Grid item xs={4} md={4} lg={4}>
                    <Paper className={fixedHeightPaper} variant="outlined">
                        <CustomList
                            itemLabel={this.contactLabel}
                            items={this.client.contacts}
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
                                    items={messages}/>
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
                        <Box display="flex" justifyContent="center"> 
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
