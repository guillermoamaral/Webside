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
		this.description = description || "";
		this.editable = true;
		this.active = true;
	}

	static adjustColor(color, amount) {
		return (
			"#" +
			color
				.replace(/^#/, "")
				.replace(/../g, (color) =>
					(
						"0" +
						Math.min(
							255,
							Math.max(0, parseInt(color, 16) + amount)
						).toString(16)
					).substr(-2)
				)
		);
	}

	static text(name, defaultValue = "", label) {
		return new Setting(name, "text", defaultValue, label);
	}

	static paragraph(name, defaultValue = "", label) {
		return new Setting(name, "paragraph", defaultValue, label);
	}

	static url(name) {
		return new Setting(name, "url", "http://server/site");
	}

	static boolean(name, defaultValue = false, label, description) {
		return new Setting(name, "boolean", defaultValue, label, description);
	}

	static number(name, defaultValue = 0, label, description) {
		return new Setting(name, "number", defaultValue, label, description);
	}

	static color(name, defaultValue, description) {
		return new Setting(name, "color", defaultValue, null, description);
	}

	static options(name, options, defaultValue) {
		const df = defaultValue || (options.length > 0 ? options[0] : null);
		const setting = new Setting(name, "options", df);
		setting.options = options;
		return setting;
	}

	static shortcut(name, defaultValue) {
		return new Setting(name, "shortcut", defaultValue);
	}

	readOnly() {
		this.editable = false;
	}

	deactivate() {
		this.active = false;
	}

	activate() {
		this.active = true;
	}

	copyFrom(setting) {
		this.type = setting.type;
		this.default = setting.defaultValue;
		this.value = setting.value;
		this.label = setting.label;
		this.description = setting.description;
		this.editable = setting.editable;
		this.active = setting.active !== false;
	}

	copy() {
		const copy = new Setting();
		copy.name = this.name;
		copy.type = this.type;
		copy.default = this.default;
		copy.value = this.value;
		copy.label = this.label;
		copy.description = this.description;
		copy.editable = this.editable;
		copy.active = this.active;
		return copy;
	}

	fromJson(value) {
		this.value = value;
		this.active = true; // if a value comes from JSON, it implies an active setting...
	}

	toJson() {
		return this.value;
	}
}

class TextStyleSetting extends Setting {
	constructor(
		name,
		color = "#00000000",
		italic = false,
		bold = false,
		label,
		description
	) {
		super(name, "textStyle", null, label, description);
		this.italic = italic;
		this.bold = bold;
		this.color = color;
	}

	copy() {
		const copy = new TextStyleSetting();
		copy.name = this.name;
		copy.color = this.color;
		copy.italic = this.italic;
		copy.bold = this.bold;
		copy.label = this.label;
		copy.description = this.description;
		copy.editable = this.editable;
		copy.active = this.active;
		return copy;
	}

	copyFrom(setting) {
		this.color = setting.color;
		this.italic = setting.italic;
		this.bold = setting.bold;
		this.label = setting.label;
		this.description = setting.description;
		this.editable = setting.editable;
		this.active = setting.active !== false;
	}

	fromJson(json) {
		this.color = json.color;
		this.italic = json.italic;
		this.bold = json.bold;
		this.active = true; // if a value comes from JSON, it implies an active setting...
	}

	toJson() {
		return { color: this.color, italic: this.italic, bold: this.bold };
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
		return this.settings.filter((s) => s instanceof Settings);
	}

	section(name) {
		return this.sections().find((s) => s.name === name);
	}

	plainSettings() {
		return this.settings.filter((s) => s instanceof Setting);
	}

	setting(name) {
		return this.plainSettings().find((s) => s.name === name);
	}

	get(name) {
		const setting = this.setting(name);
		if (setting) {
			return setting.value === null || setting.value === undefined
				? setting.default
				: setting.value;
		}
	}

	set(name, value) {
		const setting = this.setting(name);
		if (setting) setting.value = value;
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

	setSection(name, section) {
		const index = this.settings.findIndex((s) => s.name === name);
		if (index) {
			this.settings[index] = section;
		}
	}

	addText(name, defaultValue, label) {
		return this.add(Setting.text(name, defaultValue, label));
	}

	addParagraph(name, defaultValue, label) {
		return this.add(Setting.paragraph(name, defaultValue, label));
	}

	addUrl(name) {
		return this.add(Setting.url(name));
	}

	addBoolean(name, defaultValue, label, description) {
		return this.add(
			Setting.boolean(name, defaultValue, label, description)
		);
	}

	addNumber(name, defaultValue, label, description) {
		return this.add(Setting.number(name, defaultValue, label, description));
	}

	addColor(name, defaultValue, description) {
		return this.add(Setting.color(name, defaultValue, description));
	}

	addTextStyle(name, color, label, description) {
		const setting = new TextStyleSetting(
			name,
			color,
			false,
			false,
			label,
			description
		);
		return this.add(setting);
	}

	addOptions(name, options, defaultValue) {
		return this.add(Setting.options(name, options, defaultValue));
	}

	addShortcut(name, defaultValue) {
		return this.add(Setting.shortcut(name, defaultValue));
	}

	toJson() {
		var json = {};
		this.settings
			.filter((s) => s instanceof Settings || s.active)
			.forEach((s) => {
				json[s.name] = s.toJson();
			});
		return json;
	}

	fromJson(json) {
		Object.entries(json).forEach((e) => {
			const section = this.section(e[0]);
			if (section) {
				section.fromJson(e[1]);
			} else {
				const setting = this.setting(e[0]);
				if (setting) setting.fromJson(e[1]);
			}
		});
	}

	readOnly() {
		this.settings.forEach((s) => s.readOnly());
	}

	copyFrom(settings) {
		settings.settings.forEach((s) => {
			const local = this.settings.find((t) => t.name === s.name);
			if (local) local.copyFrom(s);
		});
	}

	copy() {
		const copy = new Settings(this.name, this.label);
		copy.settings = this.settings.map((s) => s.copy());
		return copy;
	}
}

export { Setting, Settings };
