import React, { Component } from "react";
import {
	Accordion,
	AccordionSummary,
	Typography,
	Box,
	Button,
	Grid,
	FormHelperText,
} from "@mui/material";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import SettingEditor from "./SettingEditor";

class SettingsEditor extends Component {
	constructor(props) {
		super(props);
		this.state = {
			error: null,
		};
	}

	apply = () => {
		const handler = this.props.onApply;
		if (handler) {
			handler(this.props.settings);
		}
	};

	resetSection = (name) => {
		const handler = this.props.onResetSection;
		if (handler) {
			handler(name);
		}
	};

	render() {
		const settings = this.props.settings;
		const error = this.state.error;
		return (
			<Grid container direction="column" justify="center" spacing={1}>
				{error && (
					<Grid item>
						<FormHelperText>{error}</FormHelperText>
					</Grid>
				)}
				<Grid item>
					<form onSubmit={this.apply}>
						{settings.sections().map((section) => (
							<Accordion key={section.name}>
								<AccordionSummary
									expandIcon={<ExpandMoreIcon />}
								>
									<Typography variant="body1">
										{section.label}
									</Typography>
								</AccordionSummary>
								<Box ml={10} mb={2}>
									{section.plainSettings().map((setting) => (
										<SettingEditor
											setting={setting}
											key={setting.name}
										/>
									))}
									{section.sections().map((subsection) => (
										<Accordion key={subsection.name}>
											<AccordionSummary
												expandIcon={<ExpandMoreIcon />}
											>
												<Typography variant="body2">
													{subsection.label}
												</Typography>
											</AccordionSummary>
											<Box ml={10} mb={2}>
												{subsection
													.plainSettings()
													.map((subsetting) => (
														<SettingEditor
															setting={subsetting}
															key={
																subsetting.name
															}
														/>
													))}
												{subsection
													.sections()
													.map((subsubsection) => (
														<Accordion
															key={
																subsubsection.name
															}
														>
															<AccordionSummary
																expandIcon={
																	<ExpandMoreIcon />
																}
															>
																<Typography variant="body2">
																	{
																		subsubsection.label
																	}
																</Typography>
															</AccordionSummary>
															<Box ml={10} mb={2}>
																{subsubsection
																	.plainSettings()
																	.map(
																		(
																			subsubsetting
																		) => (
																			<SettingEditor
																				setting={
																					subsubsetting
																				}
																				key={
																					subsubsetting.name
																				}
																			/>
																		)
																	)}
															</Box>
														</Accordion>
													))}
											</Box>
										</Accordion>
									))}
									<Box
										mt={1}
										mr={2}
										display="flex"
										direction="row"
										justifyContent="flex-end"
										alignItems="center"
									>
										<Button
											variant="outlined"
											type="submit"
											size="small"
											onClick={() =>
												this.resetSection(section.name)
											}
										>
											Reset
										</Button>
									</Box>
								</Box>
							</Accordion>
						))}
					</form>
				</Grid>
				<Grid item>
					<Box
						display="flex"
						direction="row"
						justifyContent="flex-end"
						alignItems="center"
					>
						<Button
							variant="outlined"
							type="submit"
							onClick={this.apply}
						>
							Apply
						</Button>
					</Box>
				</Grid>
			</Grid>
		);
	}
}

export default SettingsEditor;
