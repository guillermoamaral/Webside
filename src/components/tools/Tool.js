import { Component } from "react";
import ToolContainerContext from "../ToolContainerContext";

class Tool extends Component {
	static contextType = ToolContainerContext;

	aboutToClose() {}

	aboutToSelect() {}
}

export default Tool;
