import Tool from "./Tool";
import { ide } from "../IDE";

class RefactoringBrowser extends Tool {
	// Contents...
	currentClass() {
		const { selectedClass, selectedSide } = this.state;
		if (!selectedClass) return null;
		return selectedSide === "instance"
			? selectedClass
			: selectedClass.metaclass;
	}

	currentCategories = () => {
		const species = this.currentClass();
		return species ? species.categories || [] : [];
	};

	currentUsedCategories = () => {
		const species = this.currentClass();
		return species ? species.usedCategories || [] : [];
	};

	currentUsualCategories = () => {
		const species = this.currentClass();
		return species ? species.usualCategories || [] : [];
	};

	// Updating...
	async updateClass(species, forced = false) {
		try {
			if (forced || !species.definition || !species.metaclass) {
				const definition = await ide.backend.classNamed(species.name);
				Object.assign(species, definition);
				species.metaclass = await ide.backend.classNamed(
					definition.class
				);
			}
			if (forced || !species.subclasses) {
				species.subclasses = await ide.backend.subclasses(species.name);
				species.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateSubclasses(species) {
		if (!species.subclasses || species.subclasses.length === 0) return;
		try {
			const tree = await ide.backend.classTree(species.name, 10, true);
			species.subclasses.forEach((c) => {
				const retrieved = tree.subclasses.find(
					(s) => s.name === c.name
				);
				if (retrieved) {
					Object.assign(c, retrieved);
				}
			});
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateCategories(species, forced = false) {
		try {
			if (forced || !species.categories) {
				species.categories = await ide.backend.categories(species.name);
				species.categories.sort();
				species.usedCategories = await ide.backend.usedCategories(
					species.name
				);
				species.usedCategories.sort();
				species.usualCategories = await ide.backend.usualCategories(
					species.name.endsWith(" class")
				);
				species.usualCategories.sort();
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateMethod(method) {
		try {
			const retrieved = await ide.backend.method(
				method.methodClass,
				method.selector
			);

			if (retrieved) {
				Object.assign(method, retrieved);
			} else {
				method.source = "method cannot be found";
				method.ast = null;
				method.bytecodes = null;
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	// Events...
	classExpanded = async (species) => {
		await this.updateSubclasses(species);
	};

	classRenamed = (species) => {
		this.classSelected(species);
	};

	categorySelected = async (category) => {
		const selections = this.currentSelections();
		selections.category = category;
		const { species, side } = selections;
		await this.updateMethods(species, side);
		this.applySelections(selections);
	};

	categoryAdded = async (category) => {
		const selections = this.currentSelections();
		selections.category = category;
		const species = this.currentClass();
		species.categories.push(category);
		species.categories.sort();
		this.applySelections(selections);
	};

	categoryRenamed = async (category, renamed) => {
		const selections = this.currentSelections();
		const species = this.currentClass();
		await this.updateCategories(species, true);
		await this.updateMethods(species, null, null, true);
		selections.category = species.categories.find((c) => c === renamed);
		this.applySelections(selections);
	};

	categoryRemoved = async (category) => {
		const selections = this.currentSelections();
		selections.category = null;
		const species = this.currentClass();
		await this.updateCategories(species, true);
		await this.updateMethods(species, null, null, true);
		this.applySelections(selections);
	};

	methodSelected = async (method) => {
		if (method) {
			this.context.updatePageLabel(
				this.props.id,
				method.methodClass + ">>" + method.selector
			);
		}
		const selections = this.currentSelections();
		if (!method.template) {
			await this.updateMethod(method);
		}
		selections.method = method;
		this.applySelections(selections);
	};

	methodCompiled = async (method) => {
		if (!method) {
			return;
		}
		const selections = this.currentSelections();
		const target = this.currentClass();
		if (!target.categories.includes(method.category)) {
			await this.updateCategories(target, true);
		}
		var methods = target.methods;
		var index = methods
			? methods.findIndex((m) => m.selector === method.selector)
			: -1;
		if (index === -1) {
			await this.updateMethods(
				target,
				selections.variable,
				selections.access,
				true
			);
			methods = target.methods;
			index = methods.findIndex((m) => m.selector === method.selector);
		}
		methods.splice(index, 1, method);
		selections.method = method;
		this.applySelections(selections);
	};

	methodRenamed = async (method) => {
		const selections = this.currentSelections();
		const species = this.currentClass();
		await this.updateMethods(species, null, null, true);
		selections.method = species.methods.find(
			(m) => m.selector === method.selector
		);
		this.applySelections(selections);
	};

	methodRemoved = (method) => {
		const selections = this.currentSelections();
		const species = this.currentClass();
		species.methods = species.methods.filter(
			(m) => m.selector !== method.selector
		);
		selections.method = null;
		this.applySelections(selections);
	};

	methodClassified = async (method) => {
		const selections = this.currentSelections();
		const target = this.currentClass();
		await this.updateCategories(target, true);
		if (selections.category) {
			selections.category = method.category;
		}
		selections.method = method;
		this.applySelections(selections);
	};

	evaluationContext() {
		const species = this.currentClass();
		return species
			? {
					class: species.name,
			  }
			: {};
	}
}

export default RefactoringBrowser;
