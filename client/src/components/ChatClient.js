import socketio from 'socket.io-client';

class ChatClient {
    constructor(){
        this.connected = false;
        this.socket = null;
        this.me = {username: null, id: null};
        this.contacts = [];
        this.messages = {};
        this.all = {id: '.', username: '<all>'};
        this.handlers = {
            onLogged: [],
            onContactsUpdated: [],
            onMessageReceived: [],
            onMessagesSeen: [],
            onError: []
        };
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

    onEvent(event, handler, owner) {
        this.handlers[event].push([handler.bind(), owner]);
    }

    removeHandlers(owner) {
        Object.keys(this.handlers).forEach(event => this.handlers[event] = this.handlers[event].filter(h => h[1] !== owner));
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
        this.handlers.onLogged.forEach(h => h[0]());
    }

    updateContacts(users) {
        this.contacts = [this.all];
        users.filter(user => user.id !== this.me.id).forEach(user => this.contacts.push(user));
        this.handlers.onContactsUpdated.forEach(h => h[0](this.contacts));
    }

    receiveMessage(message) {
        const id = !message.to ? this.all.id : message.from.id;
        if (!this.messages[id]) {this.messages[id]={unseen: 0, messages: []}}
        this.messages[id].unseen++;
        this.messages[id].messages.push(message);
        this.handlers.onMessageReceived.forEach(h => h[0](message));
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

    unseenMessages() {
        let unseen = 0;
        Object.keys(this.messages).forEach(id => unseen += this.messages[id].unseen); 
        return unseen;
    }

    markSeenMessagesFrom(contact) {
        if (!contact) return [];
        const data = this.messages[contact.id]; 
        if (data) {data.unseen = 0};
        this.handlers.onMessagesSeen.forEach(h => h[0](contact));
    }
}

export default ChatClient;