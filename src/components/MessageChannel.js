import socketio from "socket.io-client";

class MessageChannel {
	constructor() {
		this.connected = false;
		this.socket = null;
		this.me = { username: null, id: null };
		this.all = { id: ".", username: "<all>" };
		this.peers = [];
		this.messages = {};
		this.handlers = {
			onLogged: [],
			onPeersUpdated: [],
			onMessageReceived: [],
			onMessagesSeen: [],
			onError: [],
		};
	}

	username() {
		return this.me.username;
	}

	peerNamed(name) {
		return this.peers.find((c) => c.username === name);
	}

	peerWithId(id) {
		return this.peers.find((c) => c.id === id);
	}

	onEvent(event, handler, owner) {
		this.handlers[event].push([handler.bind(), owner]);
	}

	removeHandlers(owner) {
		Object.keys(this.handlers).forEach(
			(event) =>
				(this.handlers[event] = this.handlers[event].filter(
					(h) => h[1] !== owner
				))
		);
	}

	login(url, username) {
		try {
			this.socket = socketio(url);
			this.socket.on("logged", (data) => this.logged(data));
			this.socket.on("users", (peers) => this.peersChanged(peers));
			this.socket.on("receive", (message) => this.messageReceived(message));
			this.socket.emit("login", { username: username });
			this.me = { username: username };
			this.connected = true;
		} catch (error) {
			this.connected = false;
		}
	}

	logged(data) {
		this.me.id = data.id;
		this.handlers.onLogged.forEach((h) => h[0]());
	}

	peersChanged(peers) {
		this.peers = [this.all];
		peers
			.filter((peer) => peer.id !== this.me.id)
			.forEach((peer) => this.peers.push(peer));
		this.handlers.onPeersUpdated.forEach((h) => h[0](this.peers));
	}

	messageReceived(message) {
		if (message.type === "text") {
			const id = !message.to ? this.all.id : message.from.id;
			if (!this.messages[id]) {
				this.messages[id] = { unseen: 0, messages: [] };
			}
			this.messages[id].unseen++;
			this.messages[id].messages.push(message);
		}
		this.handlers.onMessageReceived.forEach((h) => h[0](message));
	}

	sendMessage(message) {
		if (!this.messages[message.to.id]) {
			this.messages[message.to.id] = { unseen: 0, messages: [] };
		}
		this.messages[message.to.id].messages.push(message);
		if (message.to === this.all) {
			message.to = null;
		}
		this.socket.emit("send", message);
	}

	sendText(text, peer) {
		const message = {
			date: new Date(Date.now()).toUTCString(),
			from: this.me,
			to: peer,
			text: text,
			type: "text",
		};
		this.sendMessage(message);
	}

	sendDebuggerEvent(event, id) {
		const message = {
			date: new Date(Date.now()).toUTCString(),
			from: this.me,
			to: this.all,
			event: event,
			debugger: id,
			type: "debuggerEvent",
		};
		this.sendMessage(message);
	}

	messagesFrom(peer) {
		if (!peer) return [];
		const data = this.messages[peer.id];
		return data ? data.messages : [];
	}

	unseenMessagesFrom(peer) {
		if (!peer) return [];
		const data = this.messages[peer.id];
		return data ? data.unseen : 0;
	}

	unseenMessages() {
		let unseen = 0;
		Object.keys(this.messages).forEach(
			(id) => (unseen += this.messages[id].unseen)
		);
		return unseen;
	}

	markSeenMessagesFrom(peer) {
		if (!peer) return [];
		const data = this.messages[peer.id];
		if (data) {
			data.unseen = 0;
		}
		this.handlers.onMessagesSeen.forEach((h) => h[0](peer));
	}
}

export default MessageChannel;
