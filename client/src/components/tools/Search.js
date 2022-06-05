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
} from "@material-ui/core";
import { Pagination } from "@material-ui/lab";
import { IDEContext } from "../IDEContext";

class Search extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.itemsPerPage = 10;
		this.state = {
			text: "",
			results: [],
			currentPage: 1,
		};
	}

	search = async () => {
		//const results = await this.context.referencesToString(this.state.text);
		const text = this.state.text.toLowerCase();
		const results = this.context.classNames
			.filter((n) => {
				return n.toLowerCase().includes(text);
			})
			.map((n) => {
				return { title: n, type: "class", text: n };
			});
		this.setState({ text: "", results: results });
	};

	goToResult = (r) => {
		console.log(r);
	};

	render() {
		const pages = Math.ceil(this.state.results.length / this.itemsPerPage);
		const begin = (this.state.currentPage - 1) * this.itemsPerPage;
		const end = begin + this.itemsPerPage;
		const pageResults = this.state.results.slice(begin, end);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<TextField
						value={this.state.text}
						onChange={(event) => this.setState({ text: event.target.value })}
						placeholder="Search ..."
						name="text"
						variant="outlined"
						fullWidth
						margin="dense"
						autoFocus
						type="text"
						onKeyPress={(e) => {
							if (e.key === "Enter") {
								this.search();
							}
						}}
					/>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Box display="flex" justifyContent="center">
						<RadioGroup
							name="side"
							value={this.state.type}
							onChange={this.typeChanged}
							defaultValue="classes"
							row
						>
							{[
								"All",
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
							page={this.state.currentPage}
							variant="text"
							onChange={(event, page) => this.setState({ currentPage: page })}
						/>
					)}
				</Grid>
			</Grid>
		);
	}
}

export default Search;
