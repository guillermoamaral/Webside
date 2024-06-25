import React from "react";
import Tool from "./Tool";
import {
	ToggleButton,
	ToggleButtonGroup,
	TextField,
	Box,
	Typography,
	CircularProgress,
	Tooltip,
} from "@mui/material";
import { ide } from "../IDE";
import CustomList from "../controls/CustomList";

class QuickSearch extends Tool {
	constructor(props) {
		super(props);
		this.inputRef = React.createRef();
		this.typingTimer = null;
		const options = props.initialOptions || {};
		this.state = {
			text: options.text || "",
			results: [],
			searching: false,
			selectedResult: null,
			matchCase: options.matchCase || false,
			condition: options.condition || "beginning",
			type: "all",
		};
		if (options.text) {
			this.scheduleSearch();
		}
	}

	textChanged = (text) => {
		this.setState({ text: text }, this.scheduleSearch);
	};

	scheduleSearch = () => {
		clearTimeout(this.typingTimer);
		this.typingTimer = setTimeout(() => {
			this.search();
		}, 500);
	};

	search = async () => {
		const { text, matchCase, condition, type } = this.state;
		this.setState({ searching: true });
		var results = [];
		try {
			if (text.trim().length > 0) {
				results = await ide.backend.search(
					text,
					!matchCase,
					condition,
					type
				);
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
		const { type, text } = result;
		if (type === "separator") return;
		if (type === "package") {
			ide.browsePackage(text);
		}
		if (type === "class") {
			ide.browseClass(text);
		}
		if (type === "selector") {
			ide.browseImplementors(text);
		}
		if (type === "pool") {
			ide.inspectExpression(text);
		}
		if (this.props.onResultSelect) {
			const { text, matchCase, condition } = this.state;
			this.props.onResultSelect(result, { text, matchCase, condition });
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
		["class", "method", "selector", "package", "pool"].forEach((type) => {
			const group = grouped[type];
			if (group) {
				const title =
					this.titleForType(type) + " (" + group.length + ")";
				extended.push({ type: "separator", text: title });
				group.forEach((result) => extended.push(result));
			}
		});
		return extended;
	}

	toggleMatchCase = () => {
		this.setState({ matchCase: !this.state.matchCase }, () =>
			this.search()
		);
	};

	searchIn = (type) => {
		this.setState({ type: type }, () => this.search());
	};

	setCondition = (value) => {
		this.setState({ condition: value }, () => this.search());
	};

	render() {
		const { text, searching, selectedResult, matchCase, condition, type } =
			this.state;
		const results = this.extendedResults();
		const appearance = ide.settings.section("appearance");
		const color = appearance
			.section(appearance.get("mode"))
			.get("primaryColor");
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
						<Tooltip title="Match case" placement="top">
							<ToggleButton
								selected={matchCase}
								onChange={this.toggleMatchCase}
								size="small"
								value="shift"
								sx={{ width: 30 }}
								mr={1}
							>
								Aa
							</ToggleButton>
						</Tooltip>
					</Box>
					<Box ml={1}>
						<ToggleButtonGroup
							value={condition}
							exclusive
							size="small"
							onChange={(event) =>
								this.setCondition(event.target.value)
							}
						>
							<ToggleButton value={"beginning"}>a*</ToggleButton>
							<ToggleButton value={"including"}>*a*</ToggleButton>
							<ToggleButton value={"ending"}>*a</ToggleButton>
							<ToggleButton value={"similar"}>~</ToggleButton>
						</ToggleButtonGroup>
					</Box>
				</Box>
				<Box display="flex" flexDirection="row">
					<ToggleButton
						selected={type === "all"}
						onChange={() => this.searchIn("all")}
						size="small"
						value="shift"
						sx={{ minWidth: 50, borderRadius: 28 }}
						mr={1}
					>
						All
					</ToggleButton>
					<ToggleButton
						selected={type === "class"}
						onChange={() => this.searchIn("class")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Classes
					</ToggleButton>
					<ToggleButton
						selected={type === "selector"}
						onChange={() => this.searchIn("selector")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Selectors
					</ToggleButton>
					<ToggleButton
						selected={type === "package"}
						onChange={() => this.searchIn("package")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Packages
					</ToggleButton>
					<ToggleButton
						selected={type === "pool"}
						onChange={() => this.searchIn("pool")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Pools
					</ToggleButton>
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
							itemColor={(result) =>
								result.type === "separator" ? color : "default"
							}
							labelSize={(result) =>
								result.type === "separator" ? "large" : "small"
							}
							// itemIcon={(result) => {
							// 	return result.type !== "separator" ? (
							// 		<SubdirectoryArrowRightIcon
							// 			style={{ fontSize: 12 }}
							// 		/>
							// 	) : null;
							// }}
							selectedItem={selectedResult}
							onItemSelect={(r) =>
								this.setState({ selectedResult: r })
							}
							onItemDoubleClick={this.goToResult}
						/>
					</Box>
				)}
			</Box>
		);
	}
}

export default QuickSearch;
