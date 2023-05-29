class Setting extends Object {
	constructor(name, type, defaultValue, label, description) {
		super();
		this.name = name;
		this.type = type;
		this.default = defaultValue;
		this.label = label || name;
		this.description = description || name;
		this.readOnly = false;
	}

	toJson() {
		var json = {};
		json.name = this.name;
		json.type = this.type;
		json.default = this.default;
		json.label = this.label;
		json.description = this.description;
		json.readOnly = this.readOnly;
		return json;
	}

	fromJson(json) {
		this.name = json.name;
		this.type = json.type;
		this.default = json.default;
		this.label = json.label;
		this.description = json.description;
		this.readOnly = json.readOnly;
	}

	static text(name, defaultValue = "", label) {
		return new Setting(name, "text", defaultValue, label);
	}

	static url(name) {
		return new Setting(name, "url", "http://server/site");
	}

	static boolean(name) {
		return new Setting(name, "boolean", false);
	}

	static number(name) {
		return new Setting(name, "number", 0);
	}

	static color(name, defaultValue) {
		return new Setting(name, "color", defaultValue);
	}

	static options(name, options, defaultValue) {
		var setting = new Setting(name, "options");
		setting.options = options;
		setting.default =
			defaultValue || (options.length > 0 ? options[0] : null);
		return setting;
	}
}

class Settings extends Object {
	constructor(name) {
		super();
		this.name = name;
		this.settings = [];
	}

	sections() {
		return this.settings.filter((s) => s.constructor.name === "Settings");
	}

	section(name) {
		return this.sections().find((s) => s.name === name);
	}

	plainSettings() {
		return this.settings.filter((s) => s.constructor.name === "Setting");
	}

	setting(name) {
		return this.plainSettings().find((s) => s.name === name);
	}

	get(name) {
		const setting = this.setting(name);
		if (setting) {
			return setting.value || setting.default;
		}
	}

	set(name, value) {
		const setting = this.setting(name);
		if (setting) {
			setting.value = value;
		}
	}

	add(setting) {
		this.settings.push(setting);
	}

	toJson() {
		var json = {};
		json.name = this.name;
		json.settings = this.settings.map((s) => s.toJson());
		return json;
	}

	fromJson(json) {
		this.name = json.name;
		this.settings = json.settings.map((j) => {
			var s = j.settings ? new Settings() : new Setting();
			s.fromJson(j);
			return s;
		});
	}

	readOnly() {
		this.settings.forEach((s) => (s.readOnly = true));
	}
}

export { Setting, Settings };
