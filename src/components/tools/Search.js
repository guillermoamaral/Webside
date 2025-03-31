import React from "react";
import Tool from "./Tool";
import {
	Grid,
	TextField,
	Box,
	RadioGroup,
	FormControlLabel,
	Radio,
	List,
	ListItemButton,
	ListItemIcon,
	ListItemText,
	Typography,
	LinearProgress,
} from "@mui/material";
import { Pagination } from "@mui/lab";
import { ide } from "../IDE";

class Search extends Tool {
	constructor(props) {
		super(props);
		this.resultsPerPage = 10;
		this.state = {
			selectedType: "classes",
			text: "",
			results: [],
			currentPage: 1,
			searching: false,
		};
	}

	search = async () => {
		this.setState({ searching: true, results: [] });
		const text = this.state.text.toLowerCase();
		const type = this.state.selectedType;
		var results;
		switch (type) {
			case "classes":
				results = await this.searchClasses(text);
				break;
			case "selectors":
				results = await this.searchSelectors(text);
				break;
			case "string references":
				results = await this.searchStringReferences(text);
				break;
			default:
				results = [];
		}
		this.setState({
			searching: false,
			results: results,
			currentPage: 1,
		});
	};

	async searchClasses(text) {
		try {
			const names = await ide.backend.classNames();
			return names
				.filter((name) => {
					return name.toLowerCase().includes(text);
				})
				.map((name) => {
					return { title: name, type: "class", text: name };
				});
		} catch (error) {
			ide.reportError(error);
		}
	}

	async searchSelectors(text) {
		try {
			const methods = await ide.backend.methodsMatching(text);
			return methods.map((m) => {
				return {
					title: m.methodClass,
					type: "method",
					text: m.selector,
					subtext: "...",
				};
			});
		} catch (error) {
			ide.reportError(error);
		}
	}

	async searchStringReferences(text) {
		try {
			const methods = await ide.backend.stringReferences(text);
			return methods.map((m) => {
				return {
					title: m.methodClass,
					type: "method",
					text: m.selector,
				};
			});
		} catch (error) {
			ide.reportError(error);
		}
	}

	goToResult = (r) => {
		if (r.type === "class") {
			this.context.browseClass(r.text);
		}
		if (r.type === "method") {
			this.context.browseMethod({
				methodClass: r.title,
				selector: r.text,
			});
		}
	};

	resultIcon = (result) => {
		return ide.objectIcon(result, result.text);
	};

	render() {
		const { text, selectedType, results, searching, currentPage } =
			this.state;
		const k = this.resultsPerPage;
		const pages = Math.ceil(results.length / k);
		const begin = (currentPage - 1) * k;
		const end = begin + k;
		const pageResults = results.slice(begin, end);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<TextField
						value={text}
						onChange={(event) =>
							this.setState({ text: event.target.value })
						}
						size="small"
						placeholder="Search ..."
						name="text"
						variant="outlined"
						fullWidth
						margin="dense"
						autoFocus
						type="text"
						disabled={searching}
						onKeyDown={(event) => {
							if (event.key === "Enter") {
								this.search();
							}
						}}
					/>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Box display="flex" justifyContent="center">
						<RadioGroup
							name="side"
							value={selectedType}
							onChange={(event, type) =>
								this.setState({ selectedType: type })
							}
							defaultValue="classes"
							row
						>
							{[
								"Classes",
								"Selectors",
								"Pools",
								"String references",
								"Integer references",
							].map((s) => {
								return (
									<FormControlLabel
										value={s.toLowerCase()}
										control={
											<Radio
												size="small"
												color="primary"
											/>
										}
										label={s}
										key={s}
										disabled={searching}
									/>
								);
							})}
						</RadioGroup>
					</Box>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<List>
						{pageResults.map((result, index) => {
							const icon = this.resultIcon(result);
							return (
								<ListItemButton
									alignItems="flex-start"
									key={index}
									onClick={(event) => this.goToResult(result)}
								>
									{icon && (
										<ListItemIcon>{icon}</ListItemIcon>
									)}
									<ListItemText
										primary={result.title}
										secondary={
											<React.Fragment>
												<Typography
													sx={{ display: "inline" }}
													component="span"
													variant="body2"
													color="primary"
												>
													{result.type}
												</Typography>
												{": "}
												<Box
													sx={{ display: "inline" }}
													component="span"
													fontWeight="fontWeightBold"
												>
													{result.text}
												</Box>
												<Typography>
													{result.subtext || ""}
												</Typography>
											</React.Fragment>
										}
									/>
								</ListItemButton>
							);
						})}
					</List>
					{results.length > 0 && (
						<Pagination
							count={pages}
							size="medium"
							page={currentPage}
							variant="text"
							onChange={(event, page) =>
								this.setState({ currentPage: page })
							}
						/>
					)}
					{!searching && results.length === 0 && (
						<Typography variant="h6">No results</Typography>
					)}
					{searching && <LinearProgress variant="indeterminate" />}
				</Grid>
			</Grid>
		);
	}
}

export default Search;
