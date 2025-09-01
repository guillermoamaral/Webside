class StAST extends Object {
	constructor() {
		super();
		this.type = "Method";
		this.label = "Undefined";
		this.start = 0;
		this.end = 0;
		this.children = [];
	}

	fromJson(json) {
		this.type = json.type;
		this.label = json.label;
		this.start = json.start;
		this.end = json.end;
		this.children = json.children;
	}

	forEach(block) {
		this.traverse(this, block);
	}

	traverse(node, block) {
		block(node);
		if (node.children) {
			node.children.forEach((n) => {
				this.traverse(n, block);
			});
		}
	}

	nodeAt(offset) {
		let node;
		this.traverse(this, (n) => {
			if (
				n.start <= offset &&
				offset <= n.end &&
				(!node || (node.start <= n.start && n.end <= node.end))
			) {
				node = n;
			}
		});
		return node;
	}

	nodesSatisfying(condition) {
		const nodes = [];
		this.traverse(this, (node) => {
			if (condition(node)) nodes.push(node);
		});
		return nodes;
	}

	selectorInRage(start, stop) {
		let node;
		this.traverse(this, (n) => {
			if (
				n.type === "Selector" &&
				start <= n.start &&
				n.end <= stop &&
				(!node || (n.start <= node.start && node.end <= n.end))
			) {
				node = n;
			}
		});
		return node;
	}

	selectors() {
		const selectors = [];
		this.nodesSatisfying((node) => node.type === "Selector").forEach(
			(node) => {
				if (!selectors.includes(node.value)) selectors.push(node.value);
			}
		);
		return selectors;
	}
}

export default StAST;
