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
			position: "beginning",
			type: "all",
		};
	}

	textChanged = (text) => {
		this.setState({ text: text }, () => {
			clearTimeout(this.typingTimer);
			this.typingTimer = setTimeout(() => {
				this.search();
			}, 500);
		});
	};

	search = async () => {
		const { text, matchCase, position, type } = this.state;
		this.setState({ searching: true });
		var results = [];
		try {
			if (text.length > 0) {
				results = await ide.backend.search(
					text,
					!matchCase,
					position,
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

	setPosition = (value) => {
		this.setState({ position: value }, () => this.search());
	};

	render() {
		const { text, searching, selectedResult, matchCase, position, type } =
			this.state;
		const results = this.extendedResults();
		const appearance = ide.settings.section("appearance");
		const color = appearance
			.section(appearance.get("mode"))
			.section("colors")
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
							value={position}
							exclusive
							size="small"
							onChange={(event) =>
								this.setPosition(event.target.value)
							}
						>
							<ToggleButton value={"beginning"}>a*</ToggleButton>
							<ToggleButton value={"including"}>*a*</ToggleButton>
							<ToggleButton value={"ending"}>*a</ToggleButton>
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
						selected={type === "classes"}
						onChange={() => this.searchIn("classes")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Classes
					</ToggleButton>
					<ToggleButton
						selected={type === "selectors"}
						onChange={() => this.searchIn("selectors")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Selectors
					</ToggleButton>
					<ToggleButton
						selected={type === "packages"}
						onChange={() => this.searchIn("packages")}
						size="small"
						value="shift"
						sx={{ borderRadius: 28 }}
						mr={1}
					>
						Packages
					</ToggleButton>
					<ToggleButton
						selected={type === "pools"}
						onChange={() => this.searchIn("pools")}
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
