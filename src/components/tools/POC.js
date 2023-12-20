import React from "react";
import Tool from "./Tool";
import SystemBrowser from "./SystemBrowser";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {

	render() {
		return (
			<SystemBrowser showPackages={true} />
		);
	}
}

export default POC;
