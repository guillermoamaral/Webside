import Tool from "./Tool";
import { Typography, Box, Button, Link, Divider, Paper } from "@mui/material";
import SettingEditor from "../parts/SettingEditor";
import CustomTree from "../controls/CustomTree";
import CustomSplit from "../controls/CustomSplit";
import CustomPaper from "../controls/CustomPaper";
import BackendTester from "./BackendTester";
import { ide } from "../IDE";
import Scrollable from "../controls/Scrollable";
import CodeEditorBackend from "../parts/CodeEditorBackend";

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
		if (handler) handler(section.path());
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

	sampleMethod = () => {
		return `method: argument
	"This is a comment"
	| temporaryVariable |
	temporaryVariable := 123 + self unary.
	self keyword1: true keyword2: #symbol keyword3: nil.
	instanceVariable := 'string'.
	self messageWithLintError; messageWithLintInfo.
	super messageWithLintWarning.
	^SomeClass new`;
	};

	sampleAnnotations = () => {
		return [
			{
				from: 193,
				to: 213,
				type: "error",
				description: "This is a sample error",
			},
			{
				from: 215,
				to: 234,
				type: "warning",
				description: "This is a sample warning",
			},
			{
				from: 243,
				to: 265,
				type: "info",
				description: "This is a sample info",
			},
		];
	};

	settingChanged = (setting) => {
		if (setting.name === "theme") ide.applyTheme(this.props.settings);
		if (["dark", "light"].some((s) => setting.path().includes(s))) {
			this.props.settings.set("appearance.theme", "Custom");
		}
		if (setting.path().includes("appearance")) {
			if (this.codePreviewRef) {
				this.codePreviewRef.forceUpdate();
			}
		}
	};

	render() {
		const settings = this.props.settings;
		const { selectedSection, expandedSections } = this.state;
		const selectedSettings = selectedSection
			? selectedSection.plainSettings()
			: [];
		const maxLabelLength = selectedSettings.reduce(
			(max, s) => Math.max(max, s.label.length),
			0
		);
		return (
			<Box
				display="flex"
				flexDirection="column"
				sx={{ width: "100%", height: "100%" }}
			>
				<CustomSplit>
					<Box sx={{ width: "15%" }}>
						<CustomPaper>
							<CustomTree
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
							sx={{ width: "85%" }}
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
									sx={{
										width: "50%",
										height: "100%",
										marginRight: 2,
									}}
								>
									<Scrollable>
										{selectedSettings.map((setting) => (
											<Box key={setting.path()} mb={1}>
												<SettingEditor
													//showLabel={false}
													setting={setting}
													// Commented out to avoid re-rendering (until discovering why it was needed)
													onValueChange={() =>
														this.settingChanged(
															setting
														)
													}
													minLabelWidth={Math.min(
														maxLabelLength * 10,
														200
													)}
												/>
											</Box>
										))}
									</Scrollable>
								</Box>
								{selectedSection.name === "connection" && (
									<BackendTester
										url={selectedSection.get("backend")}
									/>
								)}
								{["appearance", "dark", "light"].includes(
									selectedSection.name
								) && (
									<Box
										sx={{
											width: "50%",
											height: "100%",
										}}
									>
										<Typography
											variant="subtitle1"
											sx={{ p: 1 }}
										>
											Code preview
										</Typography>
										<Paper
											variant="outlined"
											sx={{
												minWidth: 400,
												minHeight: 200,
												width: "100%",
												height: "30%",
												maxHeight: 300,
											}}
										>
											<CodeEditorBackend
												ref={(ref) => {
													this.codePreviewRef = ref;
												}}
												source={this.sampleMethod()}
												annotations={this.sampleAnnotations()}
												readOnly
												noTooltips
												settings={this.props.settings}
												inMethod
												randomProp={Math.floor(
													Math.random() * 100
												)}
											/>
										</Paper>
									</Box>
								)}
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
