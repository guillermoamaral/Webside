import { Component } from "react";
import ToolContainerContext from "../ToolContainerContext";

class Tool extends Component {
	static contextType = ToolContainerContext;

	aboutToClose() {}

	aboutToSelect() {
		this.updateCodeAssistantContext();
	}

	updateCodeAssistantContext() {}

	aboutToDeselect() {}

	updateLabel(label) {
		this.context.updatePageLabel(this.props.id, label);
	}
}

export default Tool;
