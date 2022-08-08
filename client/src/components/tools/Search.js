import React, { Component } from "react";
import {
	Grid,
	TextField,
	Box,
	RadioGroup,
	FormControlLabel,
	Radio,
	List,
	ListItem,
	ListItemText,
	Typography,
	LinearProgress,
} from "@material-ui/core";
import { Pagination } from "@material-ui/lab";
import { IDEContext } from "../IDEContext";

class Search extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.itemsPerPage = 10;
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
			const names = await this.context.api.getClassNames();
			return names
				.filter((n) => {
					return n.toLowerCase().includes(text);
				})
				.map((n) => {
					return { title: n, type: "class", text: n };
				});
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async searchSelectors(text) {
		try {
			const methods = await this.context.api.getMethodsMatching(text);
			return methods.map((m) => {
				return { title: m.methodClass, type: "method", text: m.selector };
			});
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async searchStringReferences(text) {
		try {
			const methods = await this.context.api.getStringReferences(text);
			return methods.map((m) => {
				return { title: m.methodClass, type: "method", text: m.selector };
			});
		} catch (error) {
			this.context.reportError(error);
		}
	}

	goToResult = (r) => {
		if (r.type === "class") {
			this.context.browseClass(r.text);
		}
		if (r.type === "method") {
			this.context.browseMethod({ methodClass: r.title, selector: r.text });
		}
	};

	render() {
		const { text, selectedType, results, searching, currentPage } = this.state;
		const pages = Math.ceil(results.length / this.itemsPerPage);
		const begin = (currentPage - 1) * this.itemsPerPage;
		const end = begin + this.itemsPerPage;
		const pageResults = results.slice(begin, end);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					(Warning: this component is under construction and might not work)
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<TextField
						value={text}
						onChange={(event) => this.setState({ text: event.target.value })}
						placeholder="Search ..."
						name="text"
						variant="outlined"
						fullWidth
						margin="dense"
						autoFocus
						type="text"
						disabled={searching}
						onKeyPress={(event) => {
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
							onChange={(event, type) => this.setState({ selectedType: type })}
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
										control={<Radio size="small" color="primary" />}
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
						{pageResults.map((r, i) => {
							return (
								<ListItem
									alignItems="flex-start"
									key={i}
									button
									onClick={(event) => this.goToResult(r)}
								>
									<ListItemText
										primary={r.title}
										secondary={
											<React.Fragment>
												<Typography
													sx={{ display: "inline" }}
													component="span"
													variant="body2"
													color="primary"
												>
													{r.type}
												</Typography>
												{": " + r.text + "..."}
											</React.Fragment>
										}
									/>
								</ListItem>
							);
						})}
					</List>
					{this.state.results.length > 0 && (
						<Pagination
							count={pages}
							size="medium"
							page={currentPage}
							variant="text"
							onChange={(event, page) => this.setState({ currentPage: page })}
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
