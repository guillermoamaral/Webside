import React from "react";
import Tool from "./Tool";
import {
	Grid,
	Paper,
	Box,
	Tab,
} from "@mui/material";
import CodeMerge from "../parts/CodeMerge";
import MethodVersionTable from "../parts/MethodVersionTable";
import CustomSplit from "../controls/CustomSplit";
import { ide } from "../IDE";
import Changeset from "../../model/StChangeset";

class MethodHistoryBrowser extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			changes: [],
			selectedChange: null,
		};
	}

	componentDidMount() {
		this.updateChanges();
	}

	async updateChanges() {
		try {
			let method = this.props.method;
			let changes = await ide.backend.methodHistory(method.methodClass, method.selector);
			let changeset = Changeset.fromJson(changes);
			changeset.on(ide.backend);
			this.setState({
				changes: changeset.changes,
				selectedChange: changeset.changes.length > 0 ? changeset.changes[0] : null,
			});
		} catch (error) {
			ide.reportError(error);
		}
	}

	changeSelected = async (change) => {
		this.setState({ selectedChange: change });
	};

	evaluationContext() {
		let change = this.state.selectedChange;
		return change ? { class: change.className } : {};
	}

	applyChange = async (change) => {
		try {
			await change.apply();
			let method = this.props.method;
			method = await ide.backend.method(method.methodClass, method.selector);
			this.props.method.source = method.source;
		} catch (error) {
			ide.reportError(error);
		}
		this.updateChanges();
	};

	render() {
		let { changes, selectedChange } = this.state;
		let method = this.props.method;
		console.log("rendering history browser");
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<CustomSplit mode="vertical">
					<Box sx={{ minHeight: 100, height: "35%" }}>
						<Paper variant="outlined" sx={{ height: "100%" }}>
							<MethodVersionTable
								method={method}
								changes={changes}
								selectedChange={selectedChange}
								onChangeSelect={this.changeSelected}
								onChangeApply={this.applyChange}
							/>
						</Paper>
					</Box>
					<Box
						display="flex"
						flexDirection="column"
						sx={{ height: "60%" }}
					>
						<Box>
							<Grid
								container
								spacing={1}
								sx={{ height: "100%" }}
							>
								<Grid item xs={6} md={6} lg={6}>
									<Box
										display="flex"
										alignContent="center"
										justifyContent="flex-start"
									>
										<Tab label="Version source" />
									</Box>
								</Grid>
								<Grid item xs={6} md={6} lg={6}>
									<Box
										display="flex"
										alignContent="center"
										justifyContent="flex-start"
									>
										<Tab label="Current source" />
									</Box>
								</Grid>
							</Grid>
						</Box>
						<Box flexGrow={1}>
							<Paper
								variant="outlined"
								sx={{ height: "100%", minHeight: 400 }}
							>
								<CodeMerge
									sx={{ height: "100%" }}
									context={this.evaluationContext()}
									leftCode={
										selectedChange
											? selectedChange.sourceCode()
											: ""
									}
									rightCode={
										method
											? method.source
											: ""
									}
								/>
							</Paper>
						</Box>
					</Box>
				</CustomSplit>
			</Box>
		);
	}
}

export default MethodHistoryBrowser;
