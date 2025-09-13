class ServerEventChannel {
	constructor(url, { autoConnect = true } = {}) {
		if (!("EventSource" in window)) {
			throw new Error("EventSource not available in this browser.");
		}
		this.url = url;
		this.source = null;
		this.status = "closed"; // "connecting" | "open" | "closed"
		this.typeHandlers = new Map(); // type -> Set(fn)
		this.boundListeners = new Map(); // type -> listener
		this._onOpen = null;
		this._onError = null;
		this._onMessage = null;

		if (autoConnect && url) this.connect();
	}

	connect(urlOverride) {
		if (urlOverride) this.url = urlOverride;
		if (this.source || !this.url) return this;

		this.source = new EventSource(this.url);
		this.status = "connecting";

		this.source.onopen = (e) => {
			this.status = "open";
			if (this._onOpen) this._onOpen(e);
		};

		this.source.onerror = (e) => {
			this.status =
				this.source && this.source.readyState === 2 ? "closed" : "connecting";
			if (this._onError) this._onError(e);
		};

		this.source.onmessage = (e) => {
			if (this._onMessage) this._onMessage(this._parse(e.data), e);
		};

		// re-bind registered type handlers
		for (const type of this.typeHandlers.keys()) this._bindType(type);
		return this;
	}

	close() {
		if (!this.source) return this;
		for (const type of [...this.boundListeners.keys()])
			this._unbindType(type);
		this.source.close();
		this.source = null;
		this.status = "closed";
		return this;
	}

	reconnect() {
		return this.close().connect();
	}

	setURL(url) {
		this.url = url;
		return this;
	}

	getURL() {
		return this.url;
	}

	isOpen() {
		return this.status === "open";
	}

	isConnecting() {
		return this.status === "connecting";
	}

	getStatus() {
		return this.status;
	}

	// ---------- global handlers ----------
	onOpen(fn) {
		this._onOpen = fn;
		return this;
	}

	onError(fn) {
		this._onError = fn;
		return this;
	}

	onMessage(fn) {
		this._onMessage = fn;
		return this;
	}

	// ---------- handlers by type (event: <type>) ----------
	on(type, fn) {
		if (!this.typeHandlers.has(type))
			this.typeHandlers.set(type, new Set());
		this.typeHandlers.get(type).add(fn);
		if (this.source) this._bindType(type);
		return this;
	}

	off(type, fn) {
		const set = this.typeHandlers.get(type);
		if (!set) return this;
		set.delete(fn);
		if (set.size === 0) {
			this.typeHandlers.delete(type);
			if (this.source) this._unbindType(type);
		}
		return this;
	}

	// ---------- internals ----------
	_bindType(type) {
		if (!this.source || this.boundListeners.has(type)) return;
		const listener = (e) => {
			const set = this.typeHandlers.get(type);
			if (!set || set.size === 0) return;
			const payload = this._parse(e.data);
			for (const fn of set) fn(payload, e);
		};
		this.source.addEventListener(type, listener);
		this.boundListeners.set(type, listener);
	}

	_unbindType(type) {
		const l = this.boundListeners.get(type);
		if (l && this.source) this.source.removeEventListener(type, l);
		this.boundListeners.delete(type);
	}

	_parse(s) {
		try {
			return JSON.parse(s);
		} catch {
			return s;
		}
	}
}

export default ServerEventChannel;
