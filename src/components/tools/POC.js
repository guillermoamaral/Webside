import React from "react";
import Tool from "./Tool";
import { Button } from "@mui/material";
import { ide } from "../IDE";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	constructor(props) {
		super(props);
		this.state = { options: null };
	}

	async componentDidMount() {
		if (!this.state.options) {
			const options = await ide.backend.classNames();
			this.setState({ options: options });
		}
	}

	fillForm = async () => {
		const data = await ide.fillForm({
			title: "This is a sample form dialog",
			message: "Please, fill the following form",
			inputs: [
				{
					name: "text",
					type: "text",
					label: "Text",
					description: "This is a sample text",
					required: true,
				},
				{
					name: "number",
					type: "number",
					label: "Number",
					description: "This is a sample number",
					required: true,
				},
				{
					name: "boolean",
					type: "boolean",
					label: "Boolean",
					description: "This is a sample boolean",
					defaultValue: true,
					required: true,
				},
				{
					name: "option",
					type: "options",
					label: "Option",
					options: this.state.options || [],
					description: "This is a sample option",
					required: true,
				},
			],
		});
		console.log(data);
	};

	render() {
		return <Button onClick={this.fillForm}>Open form dialog</Button>;
	}
}

export default POC;
