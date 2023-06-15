import { Component } from "react";
import { ide } from "../IDE";

class RefactoringBrowser extends Component {
	async componentDidMount() {
		await this.initializeUsualCategories();
	}

	async initializeUsualCategories() {
		try {
			this.usualCategories = await ide.api.usualCategories();
			this.usualCategories.sort();
		} catch (error) {
			ide.reportError(error);
		}
	}

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

	// Updating...
	async updateClass(species, forced = false) {
		try {
			if (forced || !species.definition || !species.metaclass) {
				const definition = await ide.api.classNamed(species.name);
				Object.assign(species, definition);
				species.metaclass = await ide.api.classNamed(definition.class);
			}
			if (forced || !species.subclasses) {
				species.subclasses = await ide.api.subclasses(species.name);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateSubclasses(species) {
		try {
			if (species.subclasses) {
				await Promise.all(
					species.subclasses.map(async (c) => {
						if (!c.subclasses) {
							c.subclasses = await ide.api.subclasses(c.name);
						}
					})
				);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateCategories(species, forced = false) {
		try {
			if (forced || !species.categories) {
				species.categories = await ide.api.categories(species.name);
				species.categories.sort();
				species.usedCategories = await ide.api.usedCategories(
					species.name
				);
				species.usedCategories.sort();
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateMethod(method) {
		try {
			const retrieved = await ide.api.method(
				method.methodClass,
				method.selector
			);
			Object.assign(method, retrieved);
			if (!retrieved) {
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
		selections.category = method.category;
		const methods = target.methods;
		const index = methods
			? methods.findIndex((m) => m.selector === method.selector)
			: -1;
		if (index === -1) {
			await this.updateMethods(target, null, null, true);
			selections.method = target.methods.find(
				(m) => m.selector === method.selector
			);
		} else {
			methods.splice(index, 1, method);
			selections.method = method;
		}
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
