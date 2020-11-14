import socketio from 'socket.io-client';

class ChatClient {
    constructor(){
        this.connected = false;
        this.socket = null;
        this.me = {username: null, id: null};
        this.contacts = [];
        this.messages = {};
        this.all = {id: '.', username: '<all>'};
    }

    username() {
        return this.me.username;
    }

    contactNamed(name) {
        return this.contacts.find(c => c.username === name);
    }

    contactWithId(id) {
        return this.contacts.find(c => c.id === id);
    }

    onLogged(handler) {
        this.onLoggedHandler = handler.bind();
    }

    onContactsUpdated(handler) {
        this.onContactsUpdatedHandler = handler.bind();
    }

    onMessageReceived(handler) {
        this.onMessageReceivedHandler = handler.bind();
    }

    onError(handler) {
        this.onErrorHandler = handler.bind();
    }

    login(url, username) {
        try {
            this.socket = socketio(url);
            this.socket.on('logged', data => this.userLogged(data));
            this.socket.on('users', users => this.updateContacts(users));
            this.socket.on('receive', message => this.receiveMessage(message));
            this.socket.emit('login', {username: username});        
            this.me = {username: username};
            this.connected = true;
        }
        catch (error) {this.connected = false}
    }

    userLogged(data) {
        this.me.id = data.id;
        if (this.onLoggedHandler) {this.onLoggedHandler()};
    }

    updateContacts(users) {
        this.contacts = [this.all];
        users.filter(user => user.id !== this.me.id).forEach(user => this.contacts.push(user));
        if (this.onContactsUpdatedHandler) {this.onContactsUpdatedHandler(this.contacts)};
    }

    receiveMessage(message) {
        const id = !message.to ? this.all.id : message.from.id;
        if (!this.messages[id]) {this.messages[id]={unseen: 0, messages: []}}
        this.messages[id].unseen++;
        this.messages[id].messages.push(message);
        if (this.onMessageReceivedHandler) {this.onMessageReceivedHandler(message)};
    }

    sendMessage(message) {
        if (message.to) {
            if (!this.messages[message.to.id]) {this.messages[message.to.id]={unseen: 0, messages: []}}
            this.messages[message.to.id].messages.push(message);
        }
        this.socket.emit('send', message);
    }

    sendText(text, contact) {
        const to = !contact || contact.username === '<all>'? null : contact; 
        const message = {
            date: new Date(Date.now()).toUTCString(),
            from: this.me,
            to: to,
            text: text
        };
        this.sendMessage(message);
    }

    messagesFrom(contact) {
        if (!contact) return [];
        const data = this.messages[contact.id]; 
        return data? data.messages : [];
    }

    unseenMessagesFrom(contact) {
        if (!contact) return [];
        const data = this.messages[contact.id]; 
        return data? data.unseen : 0;
    }

    markSeenMessagesFrom(contact) {
        if (!contact) return [];
        const data = this.messages[contact.id]; 
        if (data) {data.unseen = 0};
    }

}

export default ChatClient;