class Setting extends Object {
	constructor(name, type, defaultValue, label, description) {
		super();
		this.name = name;
		this.type = type;
		this.default = defaultValue;
		this.value = defaultValue;
		this.label = label;
		if (!this.label) {
			var raw = name || "";
			raw = raw.length > 0 ? raw[0].toUpperCase() + raw.slice(1) : raw;
			const words = raw.match(/[A-Z][a-z]+|[0-9]+/g);
			this.label = words ? words.join(" ") : raw;
		}
		this.description = description || this.label;
		this.editable = true;
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
		const setting = new Setting(name, "options");
		setting.options = options;
		setting.default =
			defaultValue || (options.length > 0 ? options[0] : null);
		return setting;
	}

	readOnly() {
		this.editable = false;
	}
}

class Settings extends Object {
	constructor(name, label) {
		super();
		this.name = name;
		this.label = label;
		if (!this.label) {
			var raw = name || "";
			raw = raw.length > 0 ? raw[0].toUpperCase() + raw.slice(1) : raw;
			const words = raw.match(/[A-Z][a-z]+|[0-9]+/g);
			this.label = words ? words.join(" ") : raw;
		}
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
		return setting;
	}

	addSection(name, label) {
		const section = new Settings(name, label);
		this.add(section);
		return section;
	}

	addText(name, defaultValue, label) {
		return this.add(Setting.text(name, defaultValue, label));
	}

	addUrl(name) {
		return this.add(Setting.url(name));
	}

	addBoolean(name) {
		return this.add(Setting.boolean(name));
	}

	addNumber(name) {
		return this.add(Setting.number(name));
	}

	addColor(name, defaultValue) {
		return this.add(Setting.color(name, defaultValue));
	}

	addOptions(name, options, defaultValue) {
		return this.add(Setting.options(name, options, defaultValue));
	}

	toJson() {
		var json = {};
		this.settings.forEach((s) => {
			if (s.constructor.name === "Settings") {
				json[s.name] = s.toJson();
			} else {
				if (s.editable) {
					json[s.name] = s.value;
				}
			}
		});
		return json;
	}

	fromJson(json) {
		Object.entries(json).forEach((e) => {
			const section = this.section(e[0]);
			if (section) {
				section.fromJson(e[1]);
			} else {
				this.set(e[0], e[1]);
			}
		});
	}

	readOnly() {
		this.settings.forEach((s) => s.readOnly());
	}
}

export { Setting, Settings };
