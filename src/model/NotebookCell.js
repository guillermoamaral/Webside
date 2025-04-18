class NotebookCell extends Object {
	constructor(type = "text", source = "") {
		super();
		this.type = type;
		this.source = source;
		this.result = null;
	}

	setSource(source) {
		if (this.source === source) return;
		this.source = source;
		this.result = null;
	}

	isCode() {
		return this.type === "code";
	}

	isText() {
		return this.type === "text";
	}

	typeLabel() {
		return this.isCode() ? "Code" : "Text";
	}

	toggleType() {
		this.type = this.isCode() ? "text" : "code";
		this.result = null;
	}
}

export default NotebookCell;
