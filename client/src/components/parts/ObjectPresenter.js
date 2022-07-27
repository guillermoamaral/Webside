import React, { Component } from "react";
import { Paper } from "@material-ui/core";
import { IDEContext } from "../IDEContext";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";

class ObjectPresenter extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			custom: false,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (!state.custom && props.object.presentation) {
			return {
				custom: true,
			};
		}
		if (state.custom && !props.object.presentation) {
			return {
				custom: false,
			};
		}
		return null;
	}

	render() {
		const custom = this.state.custom;
		const { root, object, styles } = this.props;
		const presentation = object ? object.presentation : null;
		return (
			<Paper variant="outlined" style={{ height: "100%" }}>
				{custom && presentation.type === "table" && (
					<CustomTable
						styles={styles}
						columns={presentation.columns}
						rows={presentation.rows}
					/>
				)}
				{custom && presentation.type === "html" && (
					<iframe styles={styles} srcdoc={presentation.code} />
				)}
				{!custom && (
					<CodeEditor
						context={{ object: root.id }}
						styles={styles}
						lineNumbers={false}
						source={!object ? "" : object.printString}
					/>
				)}
				{/* <Switch
					size="small"
					color="default"
					checked={custom}
					onChange={(event) => this.setState({ custom: event.target.checked })}
					inputProps={{ "aria-label": "controlled" }}
				/> */}
			</Paper>
		);
	}
}

export default ObjectPresenter;
