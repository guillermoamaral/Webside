import React from "react";
import Tool from "./Tool";
import CustomText from "../controls/CustomText";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	render() {
		return (
			<CustomText
				variant="h4"
				text={
					"default <colored>, *bold*, _italic_, <*colored bold*>, _*<italic, colored and bold>*_"
				}
			/>
		);
	}
}

export default POC;
