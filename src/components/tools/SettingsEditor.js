import React from "react";
import Tool from "./Tool";
import { Typography, Box, Button, Link, Divider } from "@mui/material";
import SettingEditor from "../parts/SettingEditor";
import FastTree from "../controls/FastTree";
import CustomSplit from "../controls/CustomSplit";
import CustomPaper from "../controls/CustomPaper";
import BackendTester from "./BackendTester";

class SettingsEditor extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			selectedSection: null,
			expandedSections: [],
			error: null,
		};
	}

	apply = () => {
		const handler = this.props.onApply;
		if (handler) handler(this.props.settings);
	};

	resetSelectedSection = (event) => {
		event.preventDefault();
		const section = this.state.selectedSection;
		const handler = this.props.onResetSection;
		if (handler) handler(section.name);
		this.forceUpdate();
	};

	sectionSelected = async (section) => {
		this.setState({ selectedSection: section });
	};

	sectionExpanded = async (section) => {
		this.setState({
			expandedSections: [...this.state.expandedSections, section],
		});
	};

	sectionCollapsed = (section) => {
		const expanded = this.state.expandedSections;
		expanded.splice(expanded.indexOf(section), 1);
		this.setState({ expandedSections: expanded });
	};

	render() {
		console.log("render settings editor")
		const settings = this.props.settings;
		const { selectedSection, expandedSections, error } = this.state;
		const selectedSettings = selectedSection
			? selectedSection.plainSettings()
			: [];
		const backendUrl =
			selectedSection && selectedSection.name === "connection"
				? selectedSection.get("backend")
				: null;
		return (
			<Box
				display="flex"
				flexDirection="column"
				sx={{ width: "100%", height: "100%" }}
			>
				<CustomSplit>
					<Box sx={{ width: "20%" }}>
						<CustomPaper>
							<FastTree
								nodes={settings.sections()}
								nodeLabel="label"
								nodeChildren={(s) => s.sections()}
								selectedNode={selectedSection}
								onNodeSelect={this.sectionSelected}
								expandedNodes={expandedSections}
								onNodeExpand={this.sectionExpanded}
								onNodeCollapse={this.sectionCollapsed}
							/>
						</CustomPaper>
					</Box>
					{selectedSection && (
						<Box
							display="flex"
							flexDirection="column"
							ml={2}
							flexGrow={1}
							sx={{ width: "80%" }}
						>
							<Typography variant="h6">
								{selectedSection.label}
							</Typography>
							<Divider />
							<Box
								mt={1}
								display="flex"
								direction="row"
								justifyContent="flex-end"
								alignItems="center"
							>
								<Link
									variant="caption"
									onClick={this.resetSelectedSection}
									color="inherit"
									underline="hover"
									href="#"
								>
									{"Reset " +
										selectedSection.label +
										" settings"}
								</Link>
							</Box>
							<Box
								m={3}
								flexGrow={1}
								display="flex"
								flexDirection="row"
							>
								<Box
									display="flex"
									flexDirection="column"
									sx={{ width: "100%", height: "90%" }}
								>
									{selectedSettings.map((setting) => (
										<Box key={setting.name} mb={1}>
											<SettingEditor
												//showLabel={false}
												setting={setting}
											/>
										</Box>
									))}
									{backendUrl && (
										<Box flexGrow={1} mt={2}>
											<BackendTester url={backendUrl} />
										</Box>
									)}
								</Box>
							</Box>
						</Box>
					)}
				</CustomSplit>
				<Divider />
				<Box
					display="flex"
					direction="row"
					justifyContent="flex-end"
					mt={1}
				>
					<Button
						variant="outlined"
						type="submit"
						onClick={this.apply}
					>
						Apply
					</Button>
				</Box>
			</Box>
		);
	}
}

export default SettingsEditor;
