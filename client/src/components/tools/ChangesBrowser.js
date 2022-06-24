import React, { Component } from "react";
import { Grid, Paper } from "@material-ui/core";
import CustomTable from "../controls/CustomTable";
//import CodeMerge from "../parts/CodeMerge";
import CodeEditor from "../parts/CodeEditor";
import { IDEContext } from "../IDEContext";

class ChangesBrowser extends Component {
	static contextType = IDEContext;
	
	constructor(props) {
		super(props);
		this.state = {
			selectedChange: null,
		};
	}

	changeSelected = (change) => {
		this.setState({ selectedChange: change });
	};

	browseClass = (change) => {
		if (change) {
			this.context.browseClass(change.class);
		}
	};

	menuOptions() {
		return [{ label: "Browse", action: this.browseClass }];
	}

	render() {
		const change = this.state.selectedChange;
		const rows = this.props.changes;
		const columns = [
			{ field: "type", label: "Type", minWidth: 150, align: "left" },
			{ field: "label", label: "Target", minWidth: 250, align: "left" },
			{ field: "package", label: "Package", minWidth: 150, align: "left" },
			{ field: "author", label: "Author", minWidth: 150, align: "center" },
			{
				field: "timestamp",
				label: "Timestamp",
				minWidth: 200,
				align: "left",
				formatter: (ts) => {
					return ts.toLocaleString("en-US");
				},
			},
		];
		const styles = this.props.styles;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined" style={{ height: 500 }}>
						<CustomTable
							style={{ height: "100%" }}
							styles={styles}
							columns={columns}
							rows={rows}
							onSelect={this.changeSelected}
							menuOptions={this.menuOptions()}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined">
						{/* <CodeMerge
							context={{ class: change ? change.class : null }}
							styles={this.props.styles}
							leftCode={change ? change.sourceCode : ""}
							rightCode={change ? change.currentSourceCode : ""}
						/> */}
						<CodeEditor
							context={{ class: change ? change.class : null }}
							styles={this.props.styles}
							lineNumbers
							source={change ? change.sourceCode : ""}
							showAccept={false}
						/>
					</Paper>
				</Grid>
			</Grid>
		);
	}
}

export default ChangesBrowser;
