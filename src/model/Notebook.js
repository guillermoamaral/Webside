import NotebookCell from "./NotebookCell";

class Notebook extends Object {
	constructor() {
		super();
		this.name = "Unnamed";
		this.cells = [];
	}

	static fromSource(source) {
		const notebook = new Notebook();
		const regex = /"([^"]*)"|([^"]+)/g;
		let match;
		while ((match = regex.exec(source)) !== null) {
			if (match[1] !== undefined) {
				const text = match[1].trim();
				if (text && !/^\\*$/.test(text)) {
					notebook.addCell("text", text);
				}
			} else if (match[2] !== undefined) {
				const code = match[2].trim();
				if (code) {
					notebook.addCell("code", code);
				}
			}
		}
		return notebook;
	}

	addCell = (type = "text", source = "", index) => {
		if (index == null) index = this.cells.length;
		const cell = new NotebookCell(type, source);
		this.cells = [
			...this.cells.slice(0, index),
			cell,
			...this.cells.slice(index),
		];
		return index;
	};

	deleteCell = (index) => {
		this.cells = [
			...this.cells.slice(0, index),
			...this.cells.slice(index + 1),
		];
	};

	reorderCells = (from, to) => {
		this.cells = [...this.cells];
		const [moved] = this.cells.splice(from, 1);
		this.cells.splice(to, 0, moved);
	};

	toSource() {
		let source = "";
		let prev;
		this.cells.forEach((cell, index) => {
			if (index > 0) source += "\r";
			if (prev && prev.isCode() && cell.isCode()) source += '"\\"\r';
			if (cell.isText()) source += '"';
			source += cell.source;
			if (cell.isText()) source += '"';
			prev = cell;
		});
		return source;
	}
}

export default Notebook;
