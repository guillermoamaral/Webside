import React from "react";
import Tool from "./Tool";
import {
	ToggleButton,
	TextField,
	Box,
	Typography,
	LinearProgress,
} from "@mui/material";
import { ide } from "../IDE";
import CustomList from "../controls/CustomList";

class QuickSearch extends Tool {
	constructor(props) {
		super(props);
		this.typingTimer = null;
		this.state = {
			text: "",
			results: [],
			searching: false,
			selectedResult: null,
			matchCase: true,
		};
	}

	textChanged = (text) => {
		this.setState({ text: text }, () => {
			clearTimeout(this.typingTimer);
			this.typingTimer = setTimeout(() => {
				this.search(this.state.text);
			}, 400);
		});
	};

	search = async (text) => {
		var results = [];
		try {
			if (text.length > 0) {
				results = await ide.backend.search(text, !this.state.matchCase);
			}
		} catch (error) {
			ide.reportError(error);
		}
		this.setState({ results: results, selectedResult: null });
	};

	goToResult = (result) => {
		if (result.type === "package") {
			this.context.browsePackage(result.text);
		}
		if (result.type === "class") {
			this.context.browseClass(result.text);
		}
		if (result.type === "selector") {
			this.context.browseImplementors(result.text);
		}
	};

	groupedResults() {
		const grouped = {};
		this.state.results.forEach((r) => {
			if (!grouped[r.type]) {
				grouped[r.type] = [];
			}
			grouped[r.type].push(r);
		});
		return grouped;
	}

	titleForType(type) {
		if (type == "selector") return "Selectors";
		if (type == "class") return "Classes";
		if (type == "pool") return "Pool dictionaries";
		if (type == "package") return "Packages";
		return "";
	}

	extendedResults() {
		const grouped = this.groupedResults();
		const extended = [];
		Object.keys(grouped).forEach((type) => {
			const title =
				this.titleForType(type) + "(" + grouped[type].length + ")";
			extended.push({ type: "separator", text: title });
			grouped[type].forEach((result) => extended.push(result));
		});
		return extended;
	}

	toggleMatchCase() {
		this.setState({ matchCase: !this.state.matchCase }, () =>
			this.search(this.state.text)
		);
	}

	render() {
		const { text, searching, selectedResult, matchCase } = this.state;
		const results = this.extendedResults();
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<Box display="flex" flexDirection="row" alignItems="center">
					<Box flexGrow={1} mr={1}>
						<TextField
							value={text}
							onChange={(event) =>
								this.textChanged(event.target.value)
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
						/>
					</Box>
					<Box>
						<ToggleButton
							selected={matchCase}
							onChange={() => this.toggleMatchCase()}
							size="small"
							value="shift"
						>
							Aa
						</ToggleButton>
					</Box>
				</Box>
				<Box flexGrow={1}>
					<CustomList
						enableFilter={false}
						items={results}
						itemLabel="text"
						labelSize={(result) =>
							result.type === "separator" ? "normal" : "small"
						}
						labelStyle={(result) =>
							result.type === "separator" ? "italic" : "normal"
						}
						selectedItem={selectedResult}
						onItemSelect={(result) =>
							this.setState({ selectedResult: result })
						}
					/>
				</Box>
				{!searching && results.length === 0 && (
					<Typography variant="h6">No results</Typography>
				)}
				{searching && <LinearProgress variant="indeterminate" />}
			</Box>
		);
	}
}

export default QuickSearch;
