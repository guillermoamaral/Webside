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
	IconButton,
} from "@mui/material";
import { ide } from "../IDE";
import CustomTree from "../controls/CustomTree";
import ArrowUpwardIcon from "@mui/icons-material/ArrowUpward";
import ArrowDownwardIcon from "@mui/icons-material/ArrowDownward";
import axios from "axios";
import GoIcon from "@mui/icons-material/Forward";

class QuickSearch extends Tool {
	constructor(props) {
		super(props);
		this.inputRef = React.createRef();
		this.typingTimer = null;
		this.types = ["class", "method", "selector", "package", "pool"];
		const options = props.initialOptions || {};
		this.state = {
			text: options.text || "",
			results: [],
			searching: false,
			selectedResult: null,
			matchCase: options.matchCase || false,
			condition: options.condition || "beginning",
			type: "all",
			sortOrder: options.sortOrder || "ascending",
			expandedTypes: [...this.types],
		};
		if (options.text) {
			this.scheduleSearch();
		}
		this.controllerRef = React.createRef();
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
		var results = [];
		try {
			if (text.trim().length > 0) {
				if (this.controllerRef.current) {
					console.log("aborting search");
					this.controllerRef.current.abort();
				}
				const controller = new AbortController();
				this.controllerRef.current = controller;
				const url = `${
					ide.backend.url
				}/search?text=${text}&ignoreCase=${!matchCase}&condition=${condition}&type=${type}`;
				this.setState({ searching: true });
				const response = await axios.get(url, {
					signal: controller.signal,
				});
				results = response.data;
			}
		} catch (error) {
			if (!axios.isCancel(error)) ide.reportError(error);
		} finally {
			this.setState(
				{
					results: results,
					searching: false,
					selectedResult: null,
					exapndedTypes: [...this.types],
				},
				() => {
					this.inputRef.current.focus();
				}
			);
		}
	};

	cancelSearch = () => {
		if (this.controllerRef.current) {
			this.controllerRef.current.abort();
			this.controllerRef.current = null;
		}
		this.setState({
			results: [],
			searching: false,
			selectedResult: null,
		});
	};

	goToResult = (result) => {
		if (result.isGroup) return;
		const { type, text } = result;
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
			const { text, matchCase, condition, sortOrder } = this.state;
			this.props.onResultSelect(result, {
				text,
				matchCase,
				condition,
				sortOrder,
			});
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
		this.types.forEach((type) => {
			const group = grouped[type];
			if (group) {
				const title =
					this.titleForType(type) + " (" + group.length + ")";
				extended.push({
					isGroup: true,
					type: type,
					text: title,
					results: this.sortResults(group),
				});
			}
		});
		return extended;
	}

	sortResults(results) {
		const order = this.state.sortOrder;
		if (order === "none") return results;
		const copy = [...results];
		if (order === "ascending")
			return copy.sort((a, b) => (a.text <= b.text ? -1 : 1));
		return copy.sort((a, b) => (a.text <= b.text ? 1 : -1));
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

	toggleSortOrder = () => {
		this.setState({
			sortOrder:
				this.state.sortOrder === "ascending"
					? "descending"
					: "ascending",
		});
	};

	resultIcon = (result) => {
		return ide.objectIcon(result, result.text);
	};

	typeExpanded = (group) => {
		this.setState({
			expandedTypes: [...this.state.expandedTypes, group.type],
		});
	};

	typeCollapsed = (group) => {
		const expanded = this.state.expandedTypes;
		expanded.splice(expanded.indexOf(group.type), 1);
		this.setState({ expandedTypes: expanded });
	};

	resultSelected = (result) => {
		this.setState({ selectedResult: result });
	};

	resultActions = (result) => {
		const actions = [];
		if (result.isGroup) return actions;
		actions.push({
			icon: <GoIcon color="primary" />,
			label: "",
			handler: this.goToResult,
		});
		return actions;
	};

	render() {
		const {
			text,
			searching,
			selectedResult,
			matchCase,
			condition,
			type,
			sortOrder,
			expandedTypes,
		} = this.state;
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
							//disabled={searching}
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
							<Tooltip
								title="Elements beginning with..."
								placement="top"
							>
								<ToggleButton value={"beginning"}>
									a*
								</ToggleButton>
							</Tooltip>
							<Tooltip
								title="Elements including..."
								placement="top"
							>
								<ToggleButton value={"including"}>
									*a*
								</ToggleButton>
							</Tooltip>
							<Tooltip
								title="Elements ending with..."
								placement="top"
							>
								<ToggleButton value={"ending"}>*a</ToggleButton>
							</Tooltip>
							<Tooltip
								title="Elements similar to..."
								placement="top"
							>
								<ToggleButton value={"similar"}>~</ToggleButton>
							</Tooltip>
						</ToggleButtonGroup>
					</Box>
				</Box>
				<Box display="flex" flexDirection="row">
					<Box display="flex" flexDirection="row" flexGrow={1}>
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
					<Box ml={1}>
						<Tooltip title="Sort order" placement="top">
							<IconButton
								size="small"
								onClick={this.toggleSortOrder}
							>
								{sortOrder === "ascending" ? (
									<ArrowUpwardIcon fontSize="small" />
								) : (
									<ArrowDownwardIcon fontSize="small" />
								)}
							</IconButton>
						</Tooltip>
					</Box>
				</Box>
				{searching && (
					<Box mt={1} display="flex" flexDirection="row">
						<CircularProgress size={20} />
						<Box ml={1} flexGrow={1}>
							<Typography variant="body">Searching...</Typography>
						</Box>
					</Box>
				)}
				{!searching && results.length === 0 && (
					<Typography mt={1} variant="body">
						{"No " +
							(this.titleForType(type).toLowerCase() ||
								"results") +
							" found"}
					</Typography>
				)}
				{!searching && results.length > 0 && (
					<Box mt={1} flexGrow={1}>
						<CustomTree
							nodes={results}
							nodeId="name"
							nodeLabel="text"
							nodeColor={(r) => (r.isGroup ? color : "default")}
							nodeIcon={this.resultIcon}
							nodeChildren="results"
							selectedNode={selectedResult}
							onNodeSelect={this.resultSelected}
							nodeActions={this.resultActions}
							expandedNodes={results.filter((r) =>
								expandedTypes.includes(r.type)
							)}
							onNodeExpand={this.typeExpanded}
							onNodeCollapse={this.typeCollapsed}
							onNodeDoubleClick={this.goToResult}
						/>
					</Box>
				)}
			</Box>
		);
	}
}

export default QuickSearch;
