import React, { Component } from "react";
import { Grid, Paper, Box, IconButton } from "@material-ui/core";
//import CodeMerge from "../parts/CodeMerge";
import CodeEditor from "../parts/CodeEditor";
import { IDEContext } from "../IDEContext";
import ChangesTable from "../parts/ChangesTable";
import DownloadIcon from "@material-ui/icons/GetApp";

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

	evaluationContext() {
		const change = this.state.selectedChange;
		return change && change.className ? { class: change.className } : {};
	}

	download = async (event) => {
		event.preventDefault();
		try {
			const ch = await this.context.api.downloadChangeset(this.props.changes);
			const blob = new Blob([ch]);
			const url = window.URL.createObjectURL(blob);
			const link = document.createElement("a");
			link.href = url;
			link.setAttribute("download", "changes.ch");
			document.body.appendChild(link);
			link.click();
			document.body.removeChild(link);
		} catch (error) {
			this.context.reportError();
		}
	};

	currentSourceCode(change) {
		if (!change) {
			return "";
		}
		let source;
		switch (change.type) {
			case "AddMethod":
				source = "...";
				break
			default:
				source = "";
		}
		return source;
	}

	render() {
		const styles = this.props.styles;
		const change = this.state.selectedChange;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Box display="flex" justifyContent="flex-end">
						<IconButton color="inherit" onClick={this.download}>
							<DownloadIcon fontSize="small" />
						</IconButton>
					</Box>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined" style={{ height: 450 }}>
						<ChangesTable
							styles={styles}
							changes={this.props.changes}
							onSelect={this.changeSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Grid container spacing={1}>
						<Grid item xs={6} md={6} lg={6}>
							<Paper variant="outlined" style={{ minHeight: 300 }}>
								{/* <CodeMerge
							context={this.evaluationContext()}
							styles={this.props.styles}
							leftCode={change ? change.sourceCode : ""}
							rightCode={change ? change.currentSourceCode : ""}
						/> */}
								<CodeEditor
									context={this.evaluationContext()}
									styles={this.props.styles}
									lineNumbers
									source={change ? change.sourceCode : ""}
									showAccept={false}
								/>
							</Paper>
						</Grid>
						<Grid item xs={6} md={6} lg={6}>
							<Paper variant="outlined" style={{ minHeight: 300 }}>
								{/* <CodeMerge
									context={this.evaluationContext()}
									styles={this.props.styles}
									leftCode={change ? change.sourceCode : ""}
									rightCode={change ? change.currentSourceCode : ""}
								/> */}
								<CodeEditor
									context={this.evaluationContext()}
									styles={this.props.styles}
									lineNumbers
									source={this.currentSourceCode(change)}
									showAccept={false}
								/>
							</Paper>
						</Grid>
					</Grid>
				</Grid>
			</Grid>
		);
	}
}

export default ChangesBrowser;
