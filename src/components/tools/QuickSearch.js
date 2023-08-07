import React from "react";
import Tool from "./Tool";
import {
	ToggleButton,
	TextField,
	Box,
	Typography,
	CircularProgress,
} from "@mui/material";
import { ide } from "../IDE";
import CustomList from "../controls/CustomList";
import SubdirectoryArrowRightIcon from "@mui/icons-material/SubdirectoryArrowRight";

class QuickSearch extends Tool {
	constructor(props) {
		super(props);
		this.inputRef = React.createRef();
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
			}, 500);
		});
	};

	search = async (text) => {
		this.setState({ searching: true });
		var results = [];
		try {
			if (text.length > 0) {
				results = await ide.backend.search(text, !this.state.matchCase);
			}
		} catch (error) {
			ide.reportError(error);
		}
		this.setState(
			{
				results: results,
				searching: false,
				selectedResult: null,
			},
			() => {
				this.inputRef.current.focus();
			}
		);
	};

	goToResult = (result) => {
		if (result.type === "separator") return;
		if (result.type === "package") {
			ide.browsePackage(result.text);
		}
		if (result.type === "class") {
			ide.browseClass(result.text);
		}
		if (result.type === "selector") {
			ide.browseImplementors(result.text);
		}
		if (this.props.onResultSelect) {
			this.props.onResultSelect();
		}
	};

	groupedResults() {
		const grouped = {};
		this.state.results.forEach((result) => {
			if (!grouped[result.type]) {
				grouped[result.type] = [];
			}
			grouped[result.type].push(result);
		});
		return grouped;
	}

	titleForType(type) {
		if (type === "selector") return "Selectors";
		if (type === "class") return "Classes";
		if (type === "pool") return "Pool dictionaries";
		if (type === "package") return "Packages";
		if (type === "method") return "Implementors";
		return "";
	}

	extendedResults() {
		const grouped = this.groupedResults();
		const extended = [];
		Object.keys(grouped).forEach((type) => {
			const title =
				this.titleForType(type) + " (" + grouped[type].length + ")";
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
							inputRef={this.inputRef}
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
				{!searching && results.length === 0 && (
					<Typography variant="body">No results</Typography>
				)}
				{searching && (
					<Box
						flexGrow={1}
						display="flex"
						flexDirection="column"
						justifyItems="center"
						alignItems="center"
					>
						<CircularProgress size={20} />
					</Box>
				)}
				{!searching && results.length > 0 && (
					<Box flexGrow={1}>
						<CustomList
							enableFilter={false}
							items={results}
							itemLabel="text"
							labelSize={(result) =>
								result.type === "separator" ? "normal" : "small"
							}
							labelStyle={(result) =>
								result.type === "separator"
									? "italic"
									: "normal"
							}
							// itemIcon={(result) => {
							// 	return result.type !== "separator" ? (
							// 		<SubdirectoryArrowRightIcon
							// 			style={{ fontSize: 16 }}
							// 		/>
							// 	) : null;
							// }}
							//selectedItem={selectedResult}
							onItemSelect={this.goToResult}
						/>
					</Box>
				)}
			</Box>
		);
	}
}

export default QuickSearch;
