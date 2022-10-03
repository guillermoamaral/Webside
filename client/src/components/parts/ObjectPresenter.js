import React, { Component } from "react";
import { Paper } from "@material-ui/core";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";

class ObjectPresenter extends Component {
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

	evaluationContext() {
		return { object: this.props.root.id };
	}

	render() {
		const custom = this.state.custom;
		const { object, styles } = this.props;
		const presentation = object ? object.presentation : null;
		return (
			<Paper variant="outlined" style={{ height: "100%" }}>
				{custom &&
					presentation.type === "table" &&
					presentation.rows.length > 100 && (
						<CustomTable
							styles={styles}
							columns={presentation.columns}
							rows={presentation.rows}
							rowsPerPage={20}
							usePagination
						/>
					)}
				{custom &&
					presentation.type === "table" &&
					presentation.rows.length <= 100 && (
						<CustomTable
							styles={styles}
							columns={presentation.columns}
							rows={presentation.rows}
						/>
					)}
				{custom && presentation.type === "html" && (
					<iframe
						styles={styles}
						srcdoc={presentation.code}
						height="100%"
						width="100%"
					/>
					// <iframe
					// 	src="http://example.com"
					// 	name="test"
					// 	height="100%"
					// 	width="100%"
					// >
					// 	You need a Frames Capable browser to view this content.
					// </iframe>
				)}
				{!custom && (
					<CodeEditor
						context={this.evaluationContext()}
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
