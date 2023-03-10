import React, { Component } from "react";
import { Grid, Paper, Box, IconButton } from "@material-ui/core";
import CodeMerge from "../parts/CodeMerge";
import CodeEditor from "../parts/CodeEditor";
import { ide } from "../IDE";
import ChangesTable from "../parts/ChangesTable";
import DownloadIcon from "@material-ui/icons/GetApp";

class ChangesBrowser extends Component {
	constructor(props) {
		super(props);
		this.state = {
			selectedChange: null,
		};
	}

	changeSelected = async (change) => {
		await change.updateCurrentSourceCode();
		this.setState({ selectedChange: change });
	};

	evaluationContext() {
		const change = this.state.selectedChange;
		return change && change.className ? { class: change.className } : {};
	}

	download = async (event) => {
		event.preventDefault();
		try {
			const ch = await ide.api.downloadChangeset(
				this.props.changeset.changes.map((ch) => {
					return ch.asJson();
				})
			);
			const blob = new Blob([ch]);
			const url = window.URL.createObjectURL(blob);
			const link = document.createElement("a");
			link.href = url;
			link.setAttribute("download", "changes.ch");
			document.body.appendChild(link);
			link.click();
			document.body.removeChild(link);
		} catch (error) {
			ide.reportError();
		}
	};

	render() {
		const styles = this.props.styles;
		const changes = this.props.changeset.changes;
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
					<Paper variant="outlined" style={{ height: 350 }}>
						<ChangesTable
							styles={styles}
							changes={changes}
							onSelect={this.changeSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<CodeMerge
						context={this.evaluationContext()}
						styles={this.props.styles}
						leftCode={change ? change.sourceCode() : ""}
						rightCode={change ? change.currentSourceCode() : ""}
					/>
					{/* <Grid container spacing={1}>
						<Grid item xs={6} md={6} lg={6}>
							<Paper
								variant="outlined"
								style={{ minHeight: 300, height: "100%" }}
							>
								<CodeEditor
									context={this.evaluationContext()}
									styles={this.props.styles}
									lineNumbers
									source={change ? change.sourceCode() : ""}
									showAccept={false}
								/>
							</Paper>
						</Grid>
						<Grid item xs={6} md={6} lg={6}>
							<Paper
								variant="outlined"
								style={{ minHeight: 300, height: "100%" }}
							>
								<CodeEditor
									context={this.evaluationContext()}
									styles={this.props.styles}
									lineNumbers
									source={
										change ? change.currentSourceCode() : ""
									}
									showAccept={false}
								/>
							</Paper>
						</Grid>
					</Grid> */}
				</Grid>
			</Grid>
		);
	}
}

export default ChangesBrowser;
