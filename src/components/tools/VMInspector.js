import React from "react";
import Tool from "./Tool";
import {
	Box,
	TextField,
	Button,
	Typography,
	Chip,
	Tooltip,
	IconButton,
	Tabs,
	Tab,
	Table,
	TableBody,
	TableCell,
	TableContainer,
	TableHead,
	TableRow,
	Link,
	Breadcrumbs,
	InputAdornment,
	LinearProgress,
	Select,
	MenuItem,
	ToggleButton,
	ToggleButtonGroup,
} from "@mui/material";
import RefreshIcon from "@mui/icons-material/Refresh";
import SearchIcon from "@mui/icons-material/Search";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import PlayArrowIcon from "@mui/icons-material/PlayArrow";
import PauseIcon from "@mui/icons-material/Pause";
import CustomSplit from "../controls/CustomSplit";
import CustomTree from "../controls/CustomTree";
import Editor from "@monaco-editor/react";
import { ide } from "../IDE";

// Monospace font stack with good Mac/Win/Linux coverage
const monoFont =
	"ui-monospace, 'SF Mono', Menlo, Monaco, 'Cascadia Code', Consolas, 'DejaVu Sans Mono', monospace";

// Utility: format byte size in human-readable form
function humanSize(bytes) {
	if (bytes == null) return "";
	if (bytes < 1024) return bytes + " B";
	if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + " KB";
	return (bytes / (1024 * 1024)).toFixed(1) + " MB";
}

// Utility: format number as hex string
function hex(n) {
	if (n == null) return "";
	let digits;
	if (typeof n === "string") {
		digits = n.replace(/^0x/i, "");
	} else {
		digits = n.toString(16);
	}
	// Remove leading zeros but keep at least 8 hex digits
	digits = digits.replace(/^0+/, "") || "0";
	if (digits.length < 8) digits = digits.padStart(8, "0");
	return "0x" + digits;
}

// =====================================================================
// ModulesPanel — list loaded native modules from /native-modules
// =====================================================================
class ModulesPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			modules: [],
			loading: false,
			filter: "",
		};
	}

	componentDidMount() {
		this.refresh();
	}

	refresh = async () => {
		this.setState({ loading: true });
		try {
			const modules = await ide.backend.nativeModules();
			this.setState({ modules, loading: false });
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	render() {
		const { modules, loading, filter } = this.state;
		const lowerFilter = filter.toLowerCase();
		const filtered = filter
			? modules.filter(
					(m) =>
						(m.name || "").toLowerCase().includes(lowerFilter) ||
						(m.path || "").toLowerCase().includes(lowerFilter)
			  )
			: modules;

		return (
			<Box sx={{ display: "flex", flexDirection: "column", height: "100%", minHeight: 0 }}>
				<Box display="flex" alignItems="center" gap={0.5} mb={0.5} flexShrink={0}>
					<TextField
						size="small"
						placeholder="Filter..."
						value={filter}
						onChange={(e) => this.setState({ filter: e.target.value })}
						sx={{ flexGrow: 1 }}
						InputProps={{
							startAdornment: (
								<InputAdornment position="start">
									<SearchIcon fontSize="small" />
								</InputAdornment>
							),
							sx: { fontSize: "0.8rem" },
						}}
					/>
					<Tooltip title="Refresh">
						<IconButton size="small" onClick={this.refresh} disabled={loading}>
							<RefreshIcon fontSize="small" />
						</IconButton>
					</Tooltip>
				</Box>
				<TableContainer sx={{ flexGrow: 1, minHeight: 0 }}>
					<Table size="small" stickyHeader sx={{ '& .MuiTableCell-root': { borderBottom: 'none' } }}>
						<TableHead>
							<TableRow>
								<TableCell sx={{ py: 0.25 }}><strong>Name</strong></TableCell>
								<TableCell sx={{ py: 0.25 }} align="right"><strong>Load</strong></TableCell>
								<TableCell sx={{ py: 0.25 }} align="right"><strong>Syms</strong></TableCell>
							</TableRow>
						</TableHead>
						<TableBody>
							{filtered.map((mod, i) => (
								<TableRow key={i} hover>
									<TableCell sx={{ py: 0.15, fontSize: "0.75rem", whiteSpace: "nowrap", overflow: "hidden", textOverflow: "ellipsis", maxWidth: 150 }}>
										<Tooltip title={mod.path || mod.name}>
											<span>{mod.name}</span>
										</Tooltip>
									</TableCell>
									<TableCell align="right" sx={{ py: 0.15, fontFamily: monoFont, fontSize: "0.7rem", whiteSpace: "nowrap" }}>
										{mod.loadAddress}
									</TableCell>
									<TableCell align="right" sx={{ py: 0.15, fontSize: "0.75rem" }}>{mod.symbolCount}</TableCell>
								</TableRow>
							))}
						</TableBody>
					</Table>
				</TableContainer>
				<Typography variant="caption" color="textSecondary" sx={{ mt: 0.25, flexShrink: 0 }}>
					{filtered.length} of {modules.length} modules
				</Typography>
			</Box>
		);
	}
}

// =====================================================================
// SymbolsPanel — search native symbols via /native-symbols?filter=
// =====================================================================
class SymbolsPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			filter: "",
			symbols: [],
			loading: false,
		};
	}

	search = async () => {
		const { filter } = this.state;
		if (!filter) return;
		this.setState({ loading: true });
		try {
			const symbols = await ide.backend.nativeSymbols(filter);
			this.setState({ symbols, loading: false });
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	handleKeyDown = (e) => {
		if (e.key === "Enter") this.search();
	};

	inspectSymbol = (sym) => {
		if (this.props.onInspectExpression) {
			this.props.onInspectExpression(sym.name);
		}
	};

	render() {
		const { filter, symbols, loading } = this.state;
		return (
			<Box sx={{ display: "flex", flexDirection: "column", height: "100%", minHeight: 0 }}>
				<Box display="flex" alignItems="center" gap={0.5} mb={0.5} flexShrink={0}>
					<TextField
						size="small"
						placeholder="Filter symbols..."
						value={filter}
						onChange={(e) => this.setState({ filter: e.target.value })}
						onKeyDown={this.handleKeyDown}
						sx={{ flexGrow: 1 }}
						InputProps={{ sx: { fontSize: "0.8rem" } }}
					/>
					<Button
						variant="contained"
						size="small"
						onClick={this.search}
						disabled={loading}
						startIcon={<SearchIcon />}
					>
						Search
					</Button>
				</Box>
				{symbols.length > 0 && (
					<TableContainer sx={{ flexGrow: 1, minHeight: 0 }}>
						<Table size="small" stickyHeader sx={{ '& .MuiTableCell-root': { borderBottom: 'none' } }}>
							<TableHead>
								<TableRow>
									<TableCell><strong>Name</strong></TableCell>
									<TableCell align="right"><strong>Address</strong></TableCell>
									<TableCell align="right"><strong>Size</strong></TableCell>
									<TableCell><strong>Type</strong></TableCell>
									<TableCell><strong>Module</strong></TableCell>
									<TableCell />
								</TableRow>
							</TableHead>
							<TableBody>
								{symbols.map((sym, i) => (
									<TableRow key={i} hover>
										<TableCell>
											<Typography variant="body2" sx={{ fontFamily: monoFont, fontSize: "0.8rem" }}>
												{sym.name}
											</Typography>
										</TableCell>
										<TableCell align="right" sx={{ fontFamily: monoFont, fontSize: "0.8rem" }}>
											{sym.address}
										</TableCell>
										<TableCell align="right">{sym.size}</TableCell>
										<TableCell>
											<Chip label={sym.type || "?"} size="small" variant="outlined" />
										</TableCell>
										<TableCell>
											<Typography variant="caption" color="textSecondary">
												{sym.module}
											</Typography>
										</TableCell>
										<TableCell>
											<Tooltip title="Inspect in Native Inspector">
												<IconButton
													size="small"
													onClick={() => this.inspectSymbol(sym)}
												>
													<ArrowForwardIcon fontSize="small" />
												</IconButton>
											</Tooltip>
										</TableCell>
									</TableRow>
								))}
							</TableBody>
						</Table>
					</TableContainer>
				)}
				{symbols.length > 0 && (
					<Typography variant="caption" color="textSecondary" sx={{ mt: 0.5 }}>
						{symbols.length} symbols found
					</Typography>
				)}
			</Box>
		);
	}
}

// =====================================================================
// DisassemblyPanel — hex byte dump of a function with IP highlighting
// =====================================================================
class DisassemblyPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			data: null,
			loading: false,
		};
	}

	componentDidMount() {
		this.load();
	}

	componentDidUpdate(prevProps) {
		if (
			prevProps.address !== this.props.address ||
			prevProps.size !== this.props.size
		) {
			this.load();
		}
	}

	load = async () => {
		const { address, size } = this.props;
		if (!address) return;
		this.setState({ loading: true });
		try {
			const data = await ide.backend.disassemble(address, size || 256);
			this.setState({ data, loading: false });
		} catch (error) {
			this.setState({ data: null, loading: false });
		}
	};

	render() {
		const { ip, label } = this.props;
		const { data, loading } = this.state;

		if (loading) return <LinearProgress />;
		if (!data) {
			return (
				<Typography
					variant="caption"
					color="textSecondary"
					sx={{ p: 1 }}
				>
					{this.props.address
						? "Failed to load disassembly"
						: "(select a frame to view disassembly)"}
				</Typography>
			);
		}

		const ipAddr = ip ? (parseInt(ip, 16) || 0) : null;
		const instructions = data.instructions || [];

		if (instructions.length > 0) {
			// Use decoded instructions
			return (
				<Box
					sx={{
						height: "100%",
						display: "flex",
						flexDirection: "column",
						minHeight: 0,
					}}
				>
					{label && (
						<Typography
							variant="caption"
							sx={{
								fontFamily: monoFont,
								px: 0.5,
								pb: 0.5,
								color: "text.secondary",
								flexShrink: 0,
							}}
						>
							{label}
						</Typography>
					)}
					<Box
						sx={{
							flexGrow: 1,
							overflow: "auto",
							fontFamily: monoFont,
							fontSize: "0.78rem",
							whiteSpace: "pre",
							backgroundColor: "action.hover",
							p: 0.5,
							borderRadius: 1,
							minHeight: 0,
						}}
					>
						{instructions.map((inst) => {
							const addr = parseInt(inst.address, 16) || 0;
							const isIP = ipAddr !== null && addr === ipAddr;
							return (
								<Box
									key={inst.address}
									sx={{
										backgroundColor: isIP
											? "rgba(255,235,59,0.3)"
											: "transparent",
										px: 0.5,
										borderRadius: isIP ? 1 : 0,
									}}
								>
									<span style={{ color: "#888" }}>
										{inst.address}
									</span>
									{"  "}
									<span style={{ color: "#666" }}>
										{(inst.bytes || "").padEnd(12)}
									</span>
									{"  "}
									{inst.text}
								</Box>
							);
						})}
					</Box>
				</Box>
			);
		}

		// Fallback: raw hex dump
		const hexStr = data.hex || "";
		const bytes = hexStr.split(" ").filter(Boolean);
		const baseAddr = parseInt(data.address, 16) || 0;
		const bytesPerLine = 16;
		const lines = [];
		for (let i = 0; i < bytes.length; i += bytesPerLine) {
			const lineAddr = baseAddr + i;
			const lineBytes = bytes.slice(i, i + bytesPerLine);
			const ascii = lineBytes
				.map((b) => {
					const c = parseInt(b, 16);
					return c >= 32 && c < 127
						? String.fromCharCode(c)
						: ".";
				})
				.join("");
			lines.push({
				address: lineAddr,
				hex: lineBytes.join(" "),
				ascii,
				containsIP:
					ipAddr !== null &&
					ipAddr >= lineAddr &&
					ipAddr < lineAddr + bytesPerLine,
			});
		}

		return (
			<Box
				sx={{
					height: "100%",
					display: "flex",
					flexDirection: "column",
					minHeight: 0,
				}}
			>
				{label && (
					<Typography
						variant="caption"
						sx={{
							fontFamily: monoFont,
							px: 0.5,
							pb: 0.5,
							color: "text.secondary",
							flexShrink: 0,
						}}
					>
						{label}
					</Typography>
				)}
				<Box
					sx={{
						flexGrow: 1,
						overflow: "auto",
						fontFamily: monoFont,
						fontSize: "0.78rem",
						whiteSpace: "pre",
						backgroundColor: "action.hover",
						p: 0.5,
						borderRadius: 1,
						minHeight: 0,
					}}
				>
					{lines.map((line) => (
						<Box
							key={line.address}
							sx={{
								backgroundColor: line.containsIP
									? "rgba(255,235,59,0.3)"
									: "transparent",
								px: 0.5,
								borderRadius: line.containsIP ? 1 : 0,
							}}
						>
							<span style={{ color: "#888" }}>
								{hex(line.address)}
							</span>
							{"  "}
							{line.hex.padEnd(48)}
							{"  "}
							<span style={{ color: "#aaa" }}>
								{line.ascii}
							</span>
						</Box>
					))}
				</Box>
			</Box>
		);
	}
}

// =====================================================================
// NativeInspectorPanel — traverse C/C++ structs via /native-inspect
// =====================================================================
class NativeInspectorPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			expression: props.initialExpression || "",
			result: null,
			loading: false,
			history: [],
		};
	}

	componentDidUpdate(prevProps) {
		if (
			this.props.initialExpression &&
			this.props.initialExpression !== prevProps.initialExpression
		) {
			this.setState(
				{ expression: this.props.initialExpression },
				this.inspect
			);
		}
	}

	inspect = async () => {
		const { expression } = this.state;
		if (!expression) return;
		this.setState({ loading: true });
		try {
			const result = await ide.backend.nativeInspect(expression);
			const history = [...this.state.history];
			if (this.state.result && this.state.result.expression) {
				history.push(this.state.result.expression);
			}
			this.setState({ result, loading: false, history });
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	navigateToField = (field) => {
		const { result } = this.state;
		if (!result) return;
		const base = result.expression;
		// For pointer fields, use -> ; for value fields, use .
		const sep = result.kind === "pointer" ? "->" : ".";
		const newExpr = base + sep + field.name;
		this.setState({ expression: newExpr }, this.inspect);
	};

	goBack = () => {
		const history = [...this.state.history];
		const prev = history.pop();
		if (prev) {
			this.setState({ expression: prev, history }, this.inspect);
		}
	};

	handleKeyDown = (e) => {
		if (e.key === "Enter") this.inspect();
	};

	breadcrumbs() {
		const { result, history } = this.state;
		if (!result) return null;
		const parts = [];
		history.forEach((expr, i) => {
			parts.push(
				<Link
					key={i}
					component="button"
					variant="body2"
					underline="hover"
					onClick={() => {
						const newHistory = history.slice(0, i);
						this.setState(
							{ expression: expr, history: newHistory },
							this.inspect
						);
					}}
				>
					{expr}
				</Link>
			);
		});
		parts.push(
			<Typography key="current" variant="body2" color="text.primary">
				{result.expression}
			</Typography>
		);
		return (
			<Breadcrumbs separator=">" sx={{ mb: 1, fontSize: "0.85rem" }}>
				{parts}
			</Breadcrumbs>
		);
	}

	render() {
		const { expression, result, loading, history } = this.state;
		return (
			<Box>
				<Box display="flex" alignItems="center" gap={1} mb={1}>
					<TextField
						size="small"
						label="C++ expression"
						placeholder="e.g. Egg::debugRuntime->_evaluator"
						value={expression}
						onChange={(e) =>
							this.setState({ expression: e.target.value })
						}
						onKeyDown={this.handleKeyDown}
						sx={{ flexGrow: 1 }}
					/>
					<Button
						variant="contained"
						size="small"
						onClick={this.inspect}
						disabled={loading}
						startIcon={<SearchIcon />}
					>
						Inspect
					</Button>
					{history.length > 0 && (
						<Button size="small" onClick={this.goBack}>
							Back
						</Button>
					)}
				</Box>

				{result && (
					<Box>
						{this.breadcrumbs()}

						{/* Header chips */}
						<Box display="flex" flexWrap="wrap" gap={1} mb={1}>
							<Chip
								label={"Type: " + result.type}
								size="small"
								color="primary"
							/>
							<Chip
								label={"Kind: " + result.kind}
								size="small"
								variant="outlined"
							/>
							<Chip
								label={"Address: " + result.address}
								size="small"
								sx={{ fontFamily: monoFont }}
							/>
							<Chip
								label={"Value: " + result.value}
								size="small"
								color="info"
								sx={{ fontFamily: monoFont }}
							/>
							{result.size > 0 && (
								<Chip
									label={
										"Size: " +
										result.size +
										" (" +
										humanSize(result.size) +
										")"
									}
									size="small"
								/>
							)}
						</Box>

						{/* Fields table */}
						{result.fields && result.fields.length > 0 && (
							<TableContainer sx={{ flexGrow: 1, minHeight: 0 }}>
								<Table size="small" stickyHeader sx={{ '& .MuiTableCell-root': { borderBottom: 'none' } }}>
									<TableHead>
										<TableRow>
											<TableCell>
												<strong>Field</strong>
											</TableCell>
											<TableCell>
												<strong>Type</strong>
											</TableCell>
											<TableCell>
												<strong>Kind</strong>
											</TableCell>
											<TableCell align="right">
												<strong>Offset</strong>
											</TableCell>
											<TableCell align="right">
												<strong>Size</strong>
											</TableCell>
											<TableCell />
										</TableRow>
									</TableHead>
									<TableBody>
										{result.fields.map((field, i) => (
											<TableRow key={i} hover>
												<TableCell>
													<Link
														component="button"
														variant="body2"
														sx={{
															fontFamily:
																monoFont,
															fontSize: "0.85rem",
														}}
														onClick={() =>
															this.navigateToField(
																field
															)
														}
													>
														{field.name}
													</Link>
												</TableCell>
												<TableCell>
													<Typography
														variant="body2"
														sx={{
															fontFamily:
																monoFont,
															fontSize: "0.8rem",
														}}
													>
														{field.type}
													</Typography>
												</TableCell>
												<TableCell>
													<Chip
														label={field.kind}
														size="small"
														variant="outlined"
														color={
															field.kind ===
															"pointer"
																? "primary"
																: field.kind ===
																  "struct" ||
																  field.kind ===
																  "class"
																? "secondary"
																: "default"
														}
													/>
												</TableCell>
												<TableCell align="right">
													+{field.offset}
												</TableCell>
												<TableCell align="right">
													{field.size}
												</TableCell>
												<TableCell>
													<Tooltip title="Inspect field">
														<IconButton
															size="small"
															onClick={() =>
																this.navigateToField(
																	field
																)
															}
														>
															<ArrowForwardIcon fontSize="small" />
														</IconButton>
													</Tooltip>
												</TableCell>
											</TableRow>
										))}
									</TableBody>
								</Table>
							</TableContainer>
						)}
						{result.fields && result.fields.length === 0 && (
							<Typography
								variant="body2"
								color="textSecondary"
								sx={{ mt: 1 }}
							>
								No fields (leaf value or opaque type)
							</Typography>
						)}
					</Box>
				)}
			</Box>
		);
	}
}

// =====================================================================
// MemoryRegionsPanel — visualize VM memory layout from /regions
// =====================================================================
class MemoryRegionsPanel extends React.Component {
	render() {
		const { regions } = this.props;
		if (!regions)
			return (
				<Typography variant="body2">
					No data. Click Refresh.
				</Typography>
			);

		const items = [];

		if (regions.oldSpace) {
			const s = regions.oldSpace;
			items.push({
				name: "Old Space",
				used: s.used,
				committed: s.committed,
				base: s.base,
				nextFree: s.nextFree,
				committedLimit: s.committedLimit,
			});
		}
		if (regions.currentSpace) {
			const s = regions.currentSpace;
			items.push({
				name: "Current Space",
				used: s.used,
				committed: s.committed,
				base: s.base,
				nextFree: s.nextFree,
				committedLimit: s.committedLimit,
			});
		}
		if (regions.codeZones) {
			for (const [key, z] of Object.entries(regions.codeZones)) {
				items.push({
					name: key,
					used: z.size,
					committed: z.size,
					base: z.start,
					nextFree: z.end,
					committedLimit: z.end,
				});
			}
		}
		if (regions.stack) {
			items.push({
				name: "Stack",
				used: regions.stack.size,
				committed: regions.stack.size,
				base: regions.stack.hardLimit,
				nextFree: regions.stack.marker,
				committedLimit: regions.stack.marker,
			});
		}

		if (items.length === 0) {
			return (
				<Typography variant="body2">
					No memory region data available.
				</Typography>
			);
		}

		return (
			<TableContainer sx={{ flexGrow: 1, minHeight: 0 }}>
				<Table size="small" stickyHeader sx={{ '& .MuiTableCell-root': { borderBottom: 'none', padding: '1px 6px', lineHeight: 1.2 } }}>
					<TableHead>
						<TableRow>
							<TableCell><strong>Region</strong></TableCell>
							<TableCell align="right"><strong>Base</strong></TableCell>
							<TableCell align="right"><strong>Next Free</strong></TableCell>
							<TableCell align="right"><strong>Committed</strong></TableCell>
							<TableCell align="right"><strong>Used</strong></TableCell>
							<TableCell align="right"><strong>Usage</strong></TableCell>
						</TableRow>
					</TableHead>
					<TableBody>
						{items.map((item) => {
							const pct =
								item.committed > 0
									? Math.round((item.used / item.committed) * 100)
									: 0;
							return (
								<TableRow key={item.name} hover>
									<TableCell sx={{ fontSize: "0.75rem", whiteSpace: "nowrap" }}>{item.name}</TableCell>
									<TableCell align="right" sx={{ fontFamily: monoFont, fontSize: "0.7rem", whiteSpace: "nowrap" }}>
										<span
											style={{ cursor: "pointer", textDecoration: "underline" }}
											onClick={() =>
												this.props.onAddressClick &&
												this.props.onAddressClick(item.base)
											}
										>
											{hex(item.base)}
										</span>
									</TableCell>
									<TableCell align="right" sx={{ fontFamily: monoFont, fontSize: "0.7rem", whiteSpace: "nowrap" }}>
										{hex(item.nextFree)}
									</TableCell>
									<TableCell align="right" sx={{ fontSize: "0.75rem", whiteSpace: "nowrap" }}>
										{humanSize(item.committed)}
									</TableCell>
									<TableCell align="right" sx={{ fontSize: "0.75rem", whiteSpace: "nowrap" }}>
										{humanSize(item.used)}
									</TableCell>
									<TableCell align="right" sx={{ minWidth: 80 }}>
										<Box display="flex" alignItems="center" gap={0.5}>
											<LinearProgress
												variant="determinate"
												value={pct}
												sx={{ flexGrow: 1, height: 6, borderRadius: 3 }}
											/>
											<Typography variant="caption">
												{pct}%
											</Typography>
										</Box>
									</TableCell>
								</TableRow>
							);
						})}
					</TableBody>
				</Table>
			</TableContainer>
		);
	}
}

// =====================================================================
// AddressClassifierPanel — classify an address via /classify
// =====================================================================
class AddressClassifierPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			address: "",
			result: null,
			loading: false,
		};
	}

	classify = async () => {
		const { address } = this.state;
		if (!address) return;
		this.setState({ loading: true });
		try {
			const result = await ide.backend.vmClassifyAddress(address);
			this.setState({ result, loading: false });
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	handleKeyDown = (e) => {
		if (e.key === "Enter") this.classify();
	};

	render() {
		const { address, result, loading } = this.state;
		return (
			<Box>
				<Box display="flex" alignItems="center" gap={1} mb={1}>
					<TextField
						size="small"
						label="Hex address"
						placeholder="0x12345678"
						value={address}
						onChange={(e) =>
							this.setState({ address: e.target.value })
						}
						onKeyDown={this.handleKeyDown}
						sx={{ flexGrow: 1 }}
					/>
					<Button
						variant="contained"
						size="small"
						onClick={this.classify}
						disabled={loading}
						startIcon={<SearchIcon />}
					>
						Classify
					</Button>
				</Box>
				{result && (
					<Box display="flex" flexWrap="wrap" gap={1}>
						{result.address && (
							<Chip
								label={"Address: " + result.address}
								size="small"
							/>
						)}
						{result.codeZone && (
							<Chip
								label={"Code Zone: " + result.codeZone}
								size="small"
								color="primary"
							/>
						)}
						{result.space && (
							<Chip
								label={"Space: " + result.space}
								size="small"
								color="secondary"
							/>
						)}
						{result.stack && (
							<Chip
								label="Stack"
								size="small"
								color="warning"
							/>
						)}
						{result.symbol && (
							<Chip
								label={
									"Symbol: " +
									result.symbol +
									(result.offset
										? " +" + result.offset
										: "")
								}
								size="small"
								color="info"
							/>
						)}
						{result.module && (
							<Chip
								label={"Module: " + result.module}
								size="small"
								variant="outlined"
							/>
						)}
						{!result.codeZone &&
							!result.space &&
							!result.stack &&
							!result.symbol && (
								<Chip
									label="Unknown region"
									size="small"
									color="default"
								/>
							)}
					</Box>
				)}
			</Box>
		);
	}
}

// =====================================================================
// OopInspectorPanel — inspect an OOP via /inspect
// =====================================================================
class OopInspectorPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			oop: props.initialOop || "",
			result: null,
			classification: null,
			typeHint: props.typeHint || null,
			bytesData: null,
			nativeBytes: null,
			vectorData: null,
			treeNodes: [],
			expandedNodes: [],
			loading: false,
			history: [],
		};
	}

	componentDidUpdate(prevProps) {
		if (
			this.props.initialOop &&
			(this.props.initialOop !== prevProps.initialOop ||
			 this.props.version !== prevProps.version)
		) {
			this.setState(
				{
					oop: this.props.initialOop,
					typeHint: this.props.typeHint || null,
				},
				this.inspect
			);
		}
	}

	isCppType(type) {
		if (!type) return false;
		if (this.isObjectPointer(type)) return false;
		if (/::/.test(type)) return true;
		if (/[<>&]/.test(type)) return true;
		if (/^(unsigned\s+)?(int|long|short|char|bool|float|double|void|size_t|u?int\d+_t)\b/.test(type)) return true;
		return false;
	}

	isObjectPointer(type) {
		if (!type) return false;
		return /Object\s*\*/.test(type) && !/vector|</.test(type);
	}

	parseVectorType(type) {
		if (!type) return null;
		const m = type.match(/std::(?:__\d+::)?vector<(.+?)>/);
		if (!m) return null;
		const elem = m[1].replace(/\s*\*\s*$/, ' *').trim();
		return { elementType: elem };
	}

	// Parse hex string from /memory?type=bytes response into byte array
	parseHexBytes(memResponse) {
		if (!memResponse || !memResponse.hex) return null;
		return memResponse.hex
			.split(" ")
			.filter((s) => s.length > 0)
			.map((s) => parseInt(s, 16));
	}

	// For reference types (&), dereference by reading the pointer at the address
	async dereferenceIfRef(addr, type) {
		if (!type || !/&\s*$/.test(type)) return addr;
		try {
			const ref = await ide.backend.vmReadMemory(addr, "uint64", 1);
			if (ref && ref.values && ref.values.length > 0) {
				return "0x" + ref.values[0].toString(16);
			}
		} catch (e) {
			// fall back to original address
		}
		return addr;
	}

	// Pretty label for a slot/element node
	prettySlotLabel(slot) {
		const idx = slot.index !== undefined ? slot.index : "";
		const raw = slot.raw ? " (" + slot.raw + ")" : "";
		if (slot.type === "SmallInteger") return idx + ": " + slot.value;
		if (slot.type === "nil") return idx + ": nil";
		if (slot.type === "object") {
			const name = slot.class || "?";
			if (slot.value !== undefined && slot.value !== null)
				return idx + ": " + slot.value + raw;
			const article = /^[aeiou]/i.test(name) ? "an" : "a";
			return idx + ": " + article + " " + name + raw;
		}
		return idx + ": " + (slot.raw || "?");
	}

	// Convert server slots into tree nodes
	isByteObjectClass(className) {
		if (!className) return false;
		return /^(String|Symbol|ByteString|ByteSymbol|WideString|Float|Double|ByteArray|LargePositiveInteger|LargeNegativeInteger)$/i.test(className);
	}

	slotsToTreeNodes(slots, pathPrefix) {
		return (slots || []).map((slot) => {
			const id = pathPrefix + "/" + slot.index;
			const expandable = slot.type === "object" && !this.isByteObjectClass(slot.class);
			return {
				id,
				raw: slot.raw,
				type: slot.type,
				class: slot.class,
				value: slot.value,
				index: slot.index,
				label: this.prettySlotLabel(slot),
				children: expandable ? [{ id: id + "/_placeholder", label: "loading...", children: [] }] : [],
			};
		});
	}

	// Handle tree node expand — lazy load slots
	handleNodeExpand = async (node) => {
		// If already loaded (children are real nodes, not placeholder), just expand
		if (node.children.length > 0 && !node.children[0].id.endsWith("/_placeholder")) {
			this.setState((prev) => ({
				expandedNodes: [...prev.expandedNodes, node],
			}));
			return;
		}
		// Fetch slots for this OOP
		if (!node.raw || node.type !== "object") return;
		try {
			const info = await ide.backend.vmInspectOop(node.raw, 50);
			if (info.isBits) {
				node.children = [];
			} else {
				node.children = this.slotsToTreeNodes(info.slots || [], node.id);
			}
		} catch (e) {
			node.children = [];
		}
		this.setState((prev) => ({
			expandedNodes: [...prev.expandedNodes, node],
			treeNodes: [...prev.treeNodes], // force re-render
		}));
	};

	handleNodeCollapse = (node) => {
		this.setState((prev) => ({
			expandedNodes: prev.expandedNodes.filter((n) => n !== node),
		}));
	};

	inspect = async () => {
		const { oop, typeHint } = this.state;
		if (!oop) return;
		this.setState({ loading: true });
		try {
			// Step 1: Classify the address
			let classification = null;
			try {
				classification = await ide.backend.vmClassifyAddress(oop);
			} catch (e) {
				// Classification is best-effort
			}

			const history = [...this.state.history];
			const prevOop = this.state.result
				? this.state.result.oop
				: this.state.classification
					? this.state.oop
					: null;
			if (prevOop && prevOop !== oop) {
				history.push(prevOop);
			}

			// Step 2: If it resolves to a native symbol, show that instead
			if (classification && classification.symbol) {
				this.setState({
					classification,
					result: null,
					bytesData: null,
					nativeBytes: null,
					vectorData: null,
					loading: false,
					history,
				});
				return;
			}

			// The DWARF getValue() already resolves references (reads the
			// pointer from the stack slot), so the address we receive is
			// the actual object address — no extra dereference needed.

			// Step 3: If it's a std::vector, read its elements
			const vecInfo = this.parseVectorType(typeHint);
			if (vecInfo) {
				let vectorData = null;
				try {
					// Read begin/end pointers (first 16 bytes of vector struct)
					const hdr = await ide.backend.vmReadMemory(oop, "uint64", 2);
					if (hdr && hdr.values && hdr.values.length >= 2) {
						const begin = hdr.values[0];
						const end = hdr.values[1];
						const elemSize = 8; // pointers are 8 bytes on ARM64
						const count = Math.max(0, Math.floor((end - begin) / elemSize));
						let elements = [];
						if (count > 0 && count < 10000) {
							const readCount = Math.min(count, 200);
							const hex = "0x" + begin.toString(16);
							const elems = await ide.backend.vmReadMemory(hex, "uint64", readCount);
							if (elems && elems.values) {
								const isObjPtr = this.isObjectPointer(vecInfo.elementType);
								const inspectPromises = elems.values.map(async (v, i) => {
									const slot = {
										index: i,
										raw: "0x" + v.toString(16),
									};
									if (isObjPtr) {
										try {
											const info = await ide.backend.vmInspectOop(slot.raw, 0);
											slot.type = info.class === "SmallInteger" ? "SmallInteger" : info.class === "UndefinedObject" ? "nil" : "object";
											slot.class = info.class;
											slot.value = info.value;
										} catch (e) { /* best-effort */ }
									} else {
										slot.type = "raw";
										slot.value = slot.raw;
									}
									return slot;
								});
								elements = await Promise.all(inspectPromises);
							}
						}
						vectorData = {
							elementType: vecInfo.elementType,
							totalCount: count,
							elements,
							truncated: count > elements.length,
						};
					}
				} catch (e) {
					// best-effort
				}
				const vecTreeNodes = this.slotsToTreeNodes(
					vectorData ? vectorData.elements : [], "vec"
				);
				this.setState({
					classification,
					result: null,
					bytesData: null,
					nativeBytes: null,
					vectorData,
					treeNodes: vecTreeNodes,
					expandedNodes: [],
					loading: false,
					history,
				});
				return;
			}

			// Step 4: If type hint is a C++ type (not Object*), show native memory
			if (this.isCppType(typeHint)) {
				let nativeBytes = null;
				try {
					nativeBytes = await ide.backend.vmReadMemory(
						oop,
						"bytes",
						256
					);
				} catch (e) {
					// best-effort
				}
				this.setState({
					classification,
					result: null,
					bytesData: null,
					nativeBytes,
					vectorData: null,
					loading: false,
					history,
				});
				return;
			}

			// Step 5: Inspect as heap object (for Object*, or no type hint)
			const result = await ide.backend.vmInspectOop(oop, 50);

			// Step 6: For byte objects, read raw bytes
			let bytesData = null;
			if (result.isBits && result.size > 0) {
				try {
					bytesData = await ide.backend.vmReadMemory(
						oop,
						"bytes",
						Math.min(result.size, 512)
					);
				} catch (e) {
					// Raw bytes read is best-effort
				}
			}

			const objTreeNodes = this.slotsToTreeNodes(result.slots || [], "obj");
			this.setState({
				result,
				classification,
				bytesData,
				nativeBytes: null,
				vectorData: null,
				treeNodes: objTreeNodes,
				expandedNodes: [],
				loading: false,
				history,
			});
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	navigateTo = (hexOop, elemType) => {
		const hint = elemType && this.isObjectPointer(elemType) ? null : elemType || null;
		this.setState({ oop: hexOop, typeHint: hint }, this.inspect);
	};

	goBack = () => {
		const history = [...this.state.history];
		const prev = history.pop();
		if (prev) {
			this.setState(
				{ oop: prev, typeHint: null, history },
				this.inspect
			);
		}
	};

	handleKeyDown = (e) => {
		if (e.key === "Enter") this.inspect();
	};

	renderClassificationBar() {
		const { classification } = this.state;
		const chips = [];
		if (classification) {
			if (classification.module) {
				chips.push(
					<Chip
						key="mod"
						label={classification.module}
						size="small"
						variant="outlined"
						sx={{ height: 20, fontSize: "0.7rem" }}
					/>
				);
			}
			if (classification.stack) {
				chips.push(
					<Chip
						key="stack"
						label="Stack"
						size="small"
						variant="outlined"
						color="warning"
						sx={{ height: 20, fontSize: "0.7rem" }}
					/>
				);
			}
		}
		if (chips.length === 0) return null;
		return (
			<Box display="flex" flexWrap="wrap" gap={0.5} mb={0.5}>
				{chips}
			</Box>
		);
	}

	renderSymbolView() {
		const { classification, typeHint } = this.state;
		if (!classification || !classification.symbol) return null;
		return (
			<Box sx={{ flexGrow: 1, overflow: "auto", minHeight: 0 }}>
				{/* Symbol name */}
				<Typography
					variant="caption"
					sx={{
						fontFamily: monoFont,
						fontWeight: "bold",
						wordBreak: "break-all",
						display: "block",
						mb: 0.5,
					}}
				>
					{classification.symbol}
				</Typography>
				{/* Info chips */}
				<Box display="flex" flexWrap="wrap" gap={0.5} mb={0.5}>
					{classification.offset !== null &&
						classification.offset !== undefined && (
							<Chip
								label={"+" + classification.offset}
								size="small"
								variant="outlined"
								sx={{ height: 20, fontSize: "0.7rem" }}
							/>
						)}
					{classification.module && (
						<Chip
							label={classification.module}
							size="small"
							variant="outlined"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{typeHint && (
						<Chip
							label={typeHint}
							size="small"
							variant="outlined"
							color="info"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{classification.stack && (
						<Chip
							label="Stack"
							size="small"
							variant="outlined"
							color="warning"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
				</Box>
			</Box>
		);
	}

	renderVectorView() {
		const { vectorData, typeHint, treeNodes, expandedNodes } = this.state;
		if (!vectorData) return null;
		return (
			<Box sx={{ flexGrow: 1, overflow: "auto", minHeight: 0, display: "flex", flexDirection: "column" }}>
				<Box display="flex" flexWrap="wrap" gap={0.5} mb={0.5}>
					<Chip
						label={
							vectorData.totalCount +
							(vectorData.totalCount === 1
								? " element"
								: " elements")
						}
						size="small"
						variant="outlined"
						sx={{ height: 20, fontSize: "0.7rem" }}
					/>
				</Box>
				{treeNodes.length > 0 && (
					<Box sx={{ flexGrow: 1, minHeight: 0, overflow: "hidden" }}>
						<CustomTree
							nodes={treeNodes}
							nodeId={(n) => n.id}
							nodeLabel={(n) => n.label}
							nodeChildren={(n) => n.children}
							onNodeExpand={this.handleNodeExpand}
							onNodeCollapse={this.handleNodeCollapse}
							expandedNodes={expandedNodes}
						/>
					</Box>
				)}
				{vectorData.truncated && (
					<Typography
						variant="caption"
						color="textSecondary"
					>
						Showing {vectorData.elements.length} of{" "}
						{vectorData.totalCount} elements
					</Typography>
				)}
			</Box>
		);
	}

	renderNativeView() {
		const { typeHint, oop, nativeBytes } = this.state;
		const bytes = this.parseHexBytes(nativeBytes);
		return (
			<Box sx={{ flexGrow: 1, overflow: "auto", minHeight: 0 }}>
				<Typography
					variant="caption"
					color="text.secondary"
					sx={{
						fontFamily: monoFont,
						display: "block",
						mb: 0.5,
					}}
				>
					at {oop}
				</Typography>
				{bytes && bytes.length > 0 && (
					<Box
						sx={{
							fontFamily: monoFont,
							fontSize: "0.72rem",
							whiteSpace: "pre",
							overflowX: "auto",
							lineHeight: 1.4,
						}}
					>
						{(() => {
							const lines = [];
							for (
								let i = 0;
								i < bytes.length;
								i += 16
							) {
								const chunk = bytes.slice(i, i + 16);
								const h = chunk
									.map((b) =>
										b
											.toString(16)
											.padStart(2, "0")
									)
									.join(" ");
								const ascii = chunk
									.map((b) =>
										b >= 32 && b < 127
											? String.fromCharCode(b)
											: "."
									)
									.join("");
								const offset = i
									.toString(16)
									.padStart(4, "0");
								lines.push(
									`${offset}  ${h.padEnd(47)}  ${ascii}`
								);
							}
							return lines.join("\n");
						})()}
					</Box>
				)}
			</Box>
		);
	}

	renderBytesView() {
		const { result, bytesData } = this.state;
		if (!result) return null;
		const bytes = this.parseHexBytes(bytesData);
		return (
			<Box sx={{ flexGrow: 1, overflow: "auto", minHeight: 0 }}>
				{this.renderObjectHeader()}
				{/* String representation for byte objects */}
				{result.string && (
					<Typography
						variant="body2"
						sx={{
							fontFamily: monoFont,
							fontSize: "0.78rem",
							backgroundColor: "action.hover",
							p: 0.5,
							borderRadius: 1,
							wordBreak: "break-all",
							mb: 0.5,
						}}
					>
						\"{result.string}\"
					</Typography>
				)}
				{/* Hex dump of raw bytes */}
				{bytes && bytes.length > 0 && (
					<Box
						sx={{
							fontFamily: monoFont,
							fontSize: "0.72rem",
							whiteSpace: "pre",
							overflowX: "auto",
							lineHeight: 1.4,
						}}
					>
						{(() => {
							const lines = [];
							for (
								let i = 0;
								i < bytes.length;
								i += 16
							) {
								const chunk = bytes.slice(i, i + 16);
								const hex = chunk
									.map((b) =>
										b
											.toString(16)
											.padStart(2, "0")
									)
									.join(" ");
								const ascii = chunk
									.map((b) =>
										b >= 32 && b < 127
											? String.fromCharCode(b)
											: "."
									)
									.join("");
								const offset = i
									.toString(16)
									.padStart(4, "0");
								lines.push(
									`${offset}  ${hex.padEnd(47)}  ${ascii}`
								);
							}
							return lines.join("\n");
						})()}
					</Box>
				)}
				{result.truncated && (
					<Typography
						variant="caption"
						color="textSecondary"
					>
						{result.size} bytes total
					</Typography>
				)}
			</Box>
		);
	}

	renderObjectHeader() {
		const { result } = this.state;
		if (!result) return null;
		return (
			<>
				<Box
					display="flex"
					flexWrap="wrap"
					gap={0.5}
					mb={0.5}
					alignItems="center"
				>
					<Typography
						variant="caption"
						sx={{
							fontFamily: monoFont,
							fontWeight: "bold",
						}}
					>
						{(() => {
							const name =
								result.class || result.type || "?";
							if (
								result.value !== undefined &&
								result.value !== null
							)
								return name;
							const article = /^[aeiou]/i.test(name)
								? "an"
								: "a";
							return article + " " + name;
						})()}
					</Typography>
					<Chip
						label={
							(result.isBits ? "Bytes: " : "Size: ") +
							(result.size ?? "?")
						}
						size="small"
						variant="outlined"
						sx={{ height: 20, fontSize: "0.7rem" }}
					/>
					{result.hash !== undefined && (
						<Chip
							label={"Hash: " + result.hash}
							size="small"
							variant="outlined"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{result.flags && (
						<Chip
							label={"Flags: " + result.flags}
							size="small"
							variant="outlined"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{result.isIndexed && (
						<Chip
							label="Indexed"
							size="small"
							variant="outlined"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{result.isNamed && (
						<Chip
							label="Named"
							size="small"
							variant="outlined"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{result.isExtended && (
						<Chip
							label="Extended"
							size="small"
							variant="outlined"
							color="warning"
							sx={{ height: 20, fontSize: "0.7rem" }}
						/>
					)}
					{result.value !== undefined &&
						result.value !== null && (
							<Chip
								label={"Val: " + result.value}
								size="small"
								variant="outlined"
								sx={{
									height: 20,
									fontSize: "0.7rem",
								}}
							/>
						)}
				</Box>
			</>
		);
	}

	render() {
		const { oop, result, classification, nativeBytes, vectorData, treeNodes, expandedNodes, loading, history, typeHint } =
			this.state;
		const isSymbolView =
			classification && classification.symbol && !result;
		const isVectorView = !result && !isSymbolView && vectorData;
		const isNativeView =
			!result && !isSymbolView && !isVectorView && this.isCppType(typeHint);
		return (
			<Box
				sx={{
					display: "flex",
					flexDirection: "column",
					height: "100%",
					minHeight: 0,
				}}
			>
				<Box
					display="flex"
					alignItems="center"
					gap={0.5}
					mb={0.5}
					flexShrink={0}
				>
					<TextField
						size="small"
						placeholder="Type"
						value={typeHint || ""}
						onChange={(e) =>
							this.setState({ typeHint: e.target.value || null })
						}
						onKeyDown={this.handleKeyDown}
						sx={{ width: "35%", flexShrink: 0 }}
						InputProps={{
							sx: {
								fontSize: "0.75rem",
								fontFamily: monoFont,
							},
						}}
					/>
					<TextField
						size="small"
						placeholder="Address (hex)"
						value={oop}
						onChange={(e) =>
							this.setState({ oop: e.target.value })
						}
						onKeyDown={this.handleKeyDown}
						sx={{ flexGrow: 1 }}
						InputProps={{
							sx: {
								fontSize: "0.8rem",
								fontFamily: monoFont,
							},
						}}
					/>
					<Button
						variant="contained"
						size="small"
						onClick={this.inspect}
						disabled={loading}
						startIcon={<SearchIcon />}
					>
						Inspect
					</Button>
					{history.length > 0 && (
						<Button size="small" onClick={this.goBack}>
							Back
						</Button>
					)}
				</Box>
				{/* Classification context bar */}
				{this.renderClassificationBar()}
				{/* Symbol view (native address) */}
				{isSymbolView && this.renderSymbolView()}
				{/* Vector view */}
				{isVectorView && this.renderVectorView()}
				{/* Native C++ type view */}
				{isNativeView && this.renderNativeView()}
				{/* Byte object view */}
				{result && result.isBits && this.renderBytesView()}
				{/* Regular heap object view */}
				{result && !result.isBits && (
					<Box
						sx={{
							flexGrow: 1,
							minHeight: 0,
							display: "flex",
							flexDirection: "column",
						}}
					>
						{this.renderObjectHeader()}

						{/* String representation */}
						{result.string && (
							<Typography
								variant="body2"
								sx={{
									fontFamily: monoFont,
									fontSize: "0.78rem",
									backgroundColor: "action.hover",
									p: 0.5,
									borderRadius: 1,
									wordBreak: "break-all",
									mb: 0.5,
								}}
							>
								"{result.string}"
							</Typography>
						)}

						{/* Slots tree */}
						{treeNodes.length > 0 && (
							<Box sx={{ flexGrow: 1, minHeight: 0, overflow: "hidden" }}>
								<CustomTree
									nodes={treeNodes}
									nodeId={(n) => n.id}
									nodeLabel={(n) => n.label}
									nodeChildren={(n) => n.children}
									onNodeExpand={this.handleNodeExpand}
									onNodeCollapse={this.handleNodeCollapse}
									expandedNodes={expandedNodes}
								/>
							</Box>
						)}
						{result.truncated && (
							<Typography
								variant="caption"
								color="textSecondary"
							>
								Showing {result.slots.length} of{" "}
								{result.totalSlots} slots
							</Typography>
						)}
					</Box>
				)}
			</Box>
		);
	}
}

// =====================================================================
// MemoryViewerPanel — raw memory read via /memory
// =====================================================================
class MemoryViewerPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			address: "",
			type: "bytes",
			count: 256,
			result: null,
			loading: false,
		};
	}

	read = async () => {
		const { address, type, count } = this.state;
		if (!address) return;
		this.setState({ loading: true });
		try {
			const result = await ide.backend.vmReadMemory(
				address,
				type,
				count
			);
			this.setState({ result, loading: false });
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	handleKeyDown = (e) => {
		if (e.key === "Enter") this.read();
	};

	formatHexDump(hexStr, ascii, bytesPerLine = 16) {
		if (!hexStr) return [];
		const hexBytes = hexStr.split(" ");
		const lines = [];
		const baseAddr = this.state.result?.address || "0x00000000";
		const base = parseInt(baseAddr, 16) || 0;

		for (let i = 0; i < hexBytes.length; i += bytesPerLine) {
			const lineHex = hexBytes
				.slice(i, i + bytesPerLine)
				.join(" ");
			const lineAscii = ascii
				? ascii.substring(i, i + bytesPerLine)
				: "";
			lines.push({
				offset: hex(base + i),
				hex: lineHex,
				ascii: lineAscii,
			});
		}
		return lines;
	}

	render() {
		const { address, type, count, result, loading } = this.state;
		return (
			<Box>
				<Box
					display="flex"
					alignItems="center"
					gap={1}
					mb={1}
					flexWrap="wrap"
				>
					<TextField
						size="small"
						label="Hex address"
						placeholder="0x12345678"
						value={address}
						onChange={(e) =>
							this.setState({ address: e.target.value })
						}
						onKeyDown={this.handleKeyDown}
						sx={{ minWidth: 160 }}
					/>
					<TextField
						size="small"
						label="Type"
						select
						value={type}
						onChange={(e) =>
							this.setState({ type: e.target.value })
						}
						SelectProps={{ native: true }}
						sx={{ minWidth: 100 }}
					>
						<option value="bytes">bytes</option>
						<option value="uint8">uint8</option>
						<option value="uint16">uint16</option>
						<option value="uint32">uint32</option>
						<option value="uint64">uint64</option>
						<option value="int8">int8</option>
						<option value="int16">int16</option>
						<option value="int32">int32</option>
						<option value="int64">int64</option>
						<option value="string">string</option>
					</TextField>
					<TextField
						size="small"
						label="Count"
						type="number"
						value={count}
						onChange={(e) =>
							this.setState({
								count: parseInt(e.target.value) || 64,
							})
						}
						sx={{ width: 80 }}
					/>
					<Button
						variant="contained"
						size="small"
						onClick={this.read}
						disabled={loading}
						startIcon={<SearchIcon />}
					>
						Read
					</Button>
				</Box>
				{result && (
					<Box>
						{/* Hex + ASCII dump for bytes type */}
						{result.hex && (
							<Box
								sx={{
									fontFamily: monoFont,
									fontSize: "0.8rem",
									whiteSpace: "pre",
									overflowX: "auto",
									backgroundColor: "action.hover",
									p: 1,
									borderRadius: 1,
									flexGrow: 1,
									minHeight: 0,
									overflowY: "auto",
								}}
							>
								{this.formatHexDump(
									result.hex,
									result.ascii
								).map((line) => (
									<Box key={line.offset}>
										<span
											style={{
												color: "#888",
											}}
										>
											{line.offset}
										</span>
										{"  "}
										{line.hex.padEnd(48)}
										{"  "}
										<span
											style={{
												color: "#6a9",
											}}
										>
											{line.ascii}
										</span>
									</Box>
								))}
							</Box>
						)}

						{/* String result */}
						{result.string !== undefined && !result.hex && (
							<Box>
								<Typography
									variant="caption"
									color="textSecondary"
								>
									String ({result.length} chars):
								</Typography>
								<Typography
									variant="body2"
									sx={{
										fontFamily: monoFont,
										backgroundColor: "action.hover",
										p: 0.5,
										borderRadius: 1,
										wordBreak: "break-all",
									}}
								>
									{result.string}
								</Typography>
							</Box>
						)}

						{/* Typed values */}
						{(result.value !== undefined || result.values) &&
							!result.hex &&
							!result.string && (
								<Box
									sx={{
										fontFamily: monoFont,
										fontSize: "0.85rem",
									}}
								>
									{result.value !== undefined && (
										<Typography
											variant="body2"
											sx={{
												fontFamily: monoFont,
											}}
										>
											{result.type}:{" "}
											{typeof result.value ===
											"object"
												? JSON.stringify(
														result.value
												  )
												: result.value}
										</Typography>
									)}
									{result.values && (
										<Box>
											{result.values.map(
												(v, i) => (
													<Typography
														key={i}
														variant="body2"
														sx={{
															fontFamily:
																"monospace",
														}}
													>
														[{i}]{" "}
														{typeof v ===
														"object"
															? JSON.stringify(
																	v
															  )
															: v}
													</Typography>
												)
											)}
										</Box>
									)}
								</Box>
							)}
					</Box>
				)}
			</Box>
		);
	}
}

// =====================================================================
// SymbolResolverPanel — resolve native symbols via /symbol
// =====================================================================
class SymbolResolverPanel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			name: "",
			result: null,
			loading: false,
		};
	}

	resolve = async () => {
		const { name } = this.state;
		if (!name) return;
		this.setState({ loading: true });
		try {
			const result = await ide.backend.vmResolveSymbol(name);
			this.setState({ result, loading: false });
		} catch (error) {
			ide.reportError(error);
			this.setState({ loading: false });
		}
	};

	handleKeyDown = (e) => {
		if (e.key === "Enter") this.resolve();
	};

	render() {
		const { name, result, loading } = this.state;
		return (
			<Box>
				<Box display="flex" alignItems="center" gap={1} mb={1}>
					<TextField
						size="small"
						label="Symbol name"
						placeholder="e.g. stStackFrame"
						value={name}
						onChange={(e) =>
							this.setState({ name: e.target.value })
						}
						onKeyDown={this.handleKeyDown}
						sx={{ flexGrow: 1 }}
					/>
					<Button
						variant="contained"
						size="small"
						onClick={this.resolve}
						disabled={loading}
						startIcon={<SearchIcon />}
					>
						Resolve
					</Button>
				</Box>
				{result && (
					<Box display="flex" flexWrap="wrap" gap={1}>
						<Chip
							label={"Name: " + result.name}
							size="small"
						/>
						<Chip
							label={"Address: " + result.address}
							size="small"
							color="primary"
							onClick={() =>
								this.props.onAddressClick &&
								this.props.onAddressClick(result.address)
							}
						/>
						<Chip
							label={"Size: " + result.size}
							size="small"
						/>
						<Chip
							label={"Module: " + result.module}
							size="small"
							variant="outlined"
						/>
						{result.value && (
							<Chip
								label={"Value: " + result.value}
								size="small"
								color="info"
							/>
						)}
					</Box>
				)}
			</Box>
		);
	}
}

// =====================================================================
// StackTracePanel — simple frame list (thread selection is in VMInspector)
// =====================================================================
class StackTracePanel extends React.Component {
	render() {
		const { frames, selectedFrame, onFrameSelect } = this.props;
		return (
			<TableContainer sx={{ flexGrow: 1 }}>
				<Table size="small" stickyHeader sx={{ '& .MuiTableCell-root': { borderBottom: 'none' } }}>
					<TableBody>
						{(frames || []).map((frame, i) => (
							<TableRow
								key={i}
								hover
								selected={
									selectedFrame &&
									selectedFrame.index === frame.index
								}
								onClick={() => onFrameSelect && onFrameSelect(frame)}
								sx={{ cursor: "pointer" }}
							>
								<TableCell
									sx={{
										py: 0.25,
										fontFamily: monoFont,
										fontSize: "0.8rem",
									}}
								>
									{frame.label}
								</TableCell>
							</TableRow>
						))}
					</TableBody>
				</Table>
			</TableContainer>
		);
	}
}

// =====================================================================
// PanelHeader — small header bar for each panel inside the split layout
// =====================================================================
function PanelHeader({ title, children }) {
	return (
		<Box
			display="flex"
			alignItems="center"
			justifyContent="space-between"
			sx={{
				px: 1,
				py: 0.25,
				borderBottom: 1,
				borderColor: "divider",
				minHeight: 28,
				flexShrink: 0,
			}}
		>
			<Typography variant="caption" fontWeight="bold">
				{title}
			</Typography>
			<Box display="flex" alignItems="center" gap={0.5}>
				{children}
			</Box>
		</Box>
	);
}

// =====================================================================
// VMInspector — main tool with three-column split layout
// Left:   Stack Trace (threads as tabs) + auxiliary panels
// Center: Code (ASM/High-level toggle) + Debug Console
// Right:  Inspection Context (bindings) + Inspection Focus (OOP detail)
// Views are connected: selecting a frame updates code & context;
// clicking an address in context updates the focus panel.
// =====================================================================
class VMInspector extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			// Thread / frame state
			debuggers: [],
			selectedDebugger: null,
			frames: [],
			selectedFrame: null,
			frameDetail: null,
			bindings: [],
			// Code view mode
			codeMode: "high-level", // "high-level" | "asm"
			inspectExpression: "",
			// Inspection focus (OOP / address clicked in context)
			inspectOop: "",
			inspectType: null,
			// Debug console
			consoleInput: "",
			consoleHistory: [],
			// Auxiliary
			regions: null,
			auxTab: 0,
			loading: false,
		};
	}

	componentDidMount() {
		this.loadDebuggers();
		this.refreshRegions();
	}

	// ---- Thread / debugger management ----

	loadDebuggers = async () => {
		try {
			const debuggers = await ide.backend.debuggers();
			const selected = debuggers.length > 0 ? debuggers[0] : null;
			this.setState({ debuggers, selectedDebugger: selected });
			if (selected) this.loadFrames(selected);
		} catch (error) {
			ide.reportError(error);
		}
	};

	loadFrames = async (dbg) => {
		this.setState({ loading: true });
		try {
			const frames = await ide.backend.debuggerFrames(dbg.id);
			const top = frames.length > 0 ? frames[0] : null;
			this.setState({ frames, selectedFrame: top, loading: false });
			if (top) this.selectFrame(top, dbg);
		} catch (error) {
			ide.reportError(error);
			this.setState({
				frames: [],
				selectedFrame: null,
				frameDetail: null,
				bindings: [],
				loading: false,
			});
		}
	};

	threadTabChanged = (event, newValue) => {
		const dbg = this.state.debuggers[newValue];
		if (dbg) {
			this.setState({ selectedDebugger: dbg });
			this.loadFrames(dbg);
		}
	};

	// ---- Frame selection → updates code & context ----

	selectFrame = async (frame, dbgOverride) => {
		const dbg = dbgOverride || this.state.selectedDebugger;
		this.setState({ selectedFrame: frame });
		if (!frame || !dbg) return;
		try {
			const detail = await ide.backend.debuggerFrame(
				dbg.id,
				frame.index
			);
			this.setState({ frameDetail: detail });
		} catch (error) {
			this.setState({ frameDetail: null });
		}
		try {
			const bindings = await ide.backend.frameBindings(
				dbg.id,
				frame.index
			);
			this.setState({ bindings: bindings || [] });
		} catch (error) {
			this.setState({ bindings: [] });
		}
	};

	// ---- Debugger controls ----

	resumeDebugger = async () => {
		const { selectedDebugger } = this.state;
		if (!selectedDebugger) return;
		try {
			await ide.backend.resumeDebugger(selectedDebugger.id);
			this.loadDebuggers();
		} catch (error) {
			ide.reportError(error);
		}
	};

	pauseDebugger = async () => {
		const { selectedDebugger } = this.state;
		if (!selectedDebugger) return;
		try {
			await ide.backend.suspendNativeDebugger(selectedDebugger.id);
			this.loadDebuggers();
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepOver = async () => {
		const { selectedDebugger, selectedFrame } = this.state;
		if (!selectedDebugger || !selectedFrame) return;
		try {
			await ide.backend.stepOverDebugger(selectedDebugger.id, selectedFrame.index);
			this.loadFrames(selectedDebugger);
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepInto = async () => {
		const { selectedDebugger, selectedFrame } = this.state;
		if (!selectedDebugger || !selectedFrame) return;
		try {
			await ide.backend.stepIntoDebugger(selectedDebugger.id, selectedFrame.index);
			this.loadFrames(selectedDebugger);
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepOut = async () => {
		const { selectedDebugger, selectedFrame } = this.state;
		if (!selectedDebugger || !selectedFrame) return;
		try {
			await ide.backend.stepOutDebugger(selectedDebugger.id, selectedFrame.index);
			this.loadFrames(selectedDebugger);
		} catch (error) {
			ide.reportError(error);
		}
	};

	reverseStepOver = async () => {
		const { selectedDebugger, selectedFrame } = this.state;
		if (!selectedDebugger || !selectedFrame) return;
		try {
			await ide.backend.reverseStepOverDebugger(selectedDebugger.id, selectedFrame.index);
			this.loadFrames(selectedDebugger);
		} catch (error) {
			ide.reportError(error);
		}
	};

	reverseStepInto = async () => {
		const { selectedDebugger, selectedFrame } = this.state;
		if (!selectedDebugger || !selectedFrame) return;
		try {
			await ide.backend.reverseStepIntoDebugger(selectedDebugger.id, selectedFrame.index);
			this.loadFrames(selectedDebugger);
		} catch (error) {
			ide.reportError(error);
		}
	};

	reverseStepOut = async () => {
		const { selectedDebugger, selectedFrame } = this.state;
		if (!selectedDebugger || !selectedFrame) return;
		try {
			await ide.backend.reverseStepOutDebugger(selectedDebugger.id, selectedFrame.index);
			this.loadFrames(selectedDebugger);
		} catch (error) {
			ide.reportError(error);
		}
	};

	// ---- Other handlers ----

	refreshRegions = async () => {
		try {
			const regions = await ide.backend.vmRegions();
			this.setState({ regions });
		} catch (error) {
			// not available — ignore
		}
	};

	handleAddressClick = (address, typeHint) => {
		const hexAddr = typeof address === "string" ? address : hex(address);
		this.setState({
			inspectOop: hexAddr,
			inspectType: typeHint || null,
			inspectVersion: (this.state.inspectVersion || 0) + 1,
		});
	};

	handleInspectExpression = (expression) => {
		this.setState({ consoleInput: expression });
	};

	// ---- Debug console ----

	consoleEval = async () => {
		const { consoleInput, consoleHistory } = this.state;
		if (!consoleInput.trim()) return;
		const entry = { input: consoleInput, output: null, error: null };
		try {
			const result = await ide.backend.nativeInspect(consoleInput);
			entry.output =
				result.value !== undefined
					? String(result.value)
					: JSON.stringify(result);
		} catch (error) {
			entry.error = error.message || String(error);
		}
		this.setState({
			consoleHistory: [...consoleHistory, entry],
			consoleInput: "",
		});
	};

	// ---- Code panel content ----

	renderCodePanel() {
		const { codeMode, frameDetail } = this.state;
		if (codeMode === "asm") {
			const functionAddr = frameDetail?.functionAddress || null;
			const ip = frameDetail?.ip || null;
			return (
				<DisassemblyPanel
					address={functionAddr}
					ip={ip}
				/>
			);
		}
		// High-level: show source from selected frame
		const source =
			frameDetail?.method?.source || "(select a frame to view source)";
		return (
			<Box sx={{ height: "100%", display: "flex", flexDirection: "column" }}>
				<Box sx={{ flexGrow: 1, minHeight: 0 }}>
					<Editor
						height="100%"
						language="cpp"
						value={source}
						theme="vs-dark"
						options={{
							readOnly: true,
							minimap: { enabled: false },
							lineNumbers: "on",
							scrollBeyondLastLine: false,
							fontFamily: monoFont,
							fontSize: 13,
							renderLineHighlight: "none",
							overviewRulerLanes: 0,
							folding: false,
							glyphMargin: false,
							domReadOnly: true,
						}}
					/>
				</Box>
			</Box>
		);
	}

	highlightSource(source, interval) {
		const from = interval.start || interval.from || 0;
		const to = interval.end || interval.to || from;
		if (from === 0 && to === 0) return source;
		const start = Math.max(0, from - 1);
		const end = Math.min(source.length, to);
		return (
			<>
				{source.substring(0, start)}
				<span
					style={{
						backgroundColor: "rgba(255,235,59,0.4)",
						borderRadius: 2,
					}}
				>
					{source.substring(start, end)}
				</span>
				{source.substring(end)}
			</>
		);
	}

	render() {
		const {
			debuggers,
			selectedDebugger,
			frames,
			selectedFrame,
			codeMode,
			bindings,
			inspectOop,
			inspectType,
			inspectVersion,
			consoleInput,
			consoleHistory,
			regions,
			auxTab,
		} = this.state;

		const threadIndex = selectedDebugger
			? debuggers.indexOf(selectedDebugger)
			: 0;

		const hasDebugger = !!selectedDebugger;
		const hasFrame = !!selectedFrame;

		return (
			<Box sx={{ height: "100%", width: "100%", display: "flex", flexDirection: "column" }}>
				{/* Debugger control toolbar */}
				<Box display="flex" alignItems="center" gap={0.25} sx={{ flexShrink: 0, px: 0.5, py: 0.25 }}>
					<Tooltip title="Resume">
						<span>
							<IconButton size="small" onClick={this.resumeDebugger} disabled={!hasDebugger}>
								<PlayArrowIcon fontSize="small" />
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip title="Pause">
						<span>
							<IconButton size="small" onClick={this.pauseDebugger} disabled={!hasDebugger}>
								<PauseIcon fontSize="small" />
							</IconButton>
						</span>
					</Tooltip>
					<Box sx={{ width: '1px', height: 20, bgcolor: 'divider', mx: 0.5 }} />
					<Tooltip title="Step Over">
						<span>
							<IconButton size="small" onClick={this.stepOver} disabled={!hasFrame}>
								<svg viewBox="0 0 16 16" width="18" height="18" fill="none"><path d="M2 11 C2 3 14 3 14 11" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"/><path d="M11.5 8.5 L14 11 L11.5 13.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><circle cx="8" cy="14" r="1.3" fill="currentColor"/></svg>
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip title="Step Into">
						<span>
							<IconButton size="small" onClick={this.stepInto} disabled={!hasFrame}>
								<svg viewBox="0 0 16 16" width="18" height="18" fill="none"><path d="M3 3 H8 V10" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M5.5 7.5 L8 10 L10.5 7.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M3 14 H13" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"/></svg>
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip title="Step Out">
						<span>
							<IconButton size="small" onClick={this.stepOut} disabled={!hasFrame}>
								<svg viewBox="0 0 16 16" width="18" height="18" fill="none"><path d="M8 12 V5 H13" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M10.5 2.5 L13 5 L10.5 7.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M3 14 H13" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"/></svg>
							</IconButton>
						</span>
					</Tooltip>
					<Box sx={{ width: '1px', height: 20, bgcolor: 'divider', mx: 0.5 }} />
					<Tooltip title="Reverse Step Over">
						<span>
							<IconButton size="small" onClick={this.reverseStepOver} disabled={!hasFrame}>
								<svg viewBox="0 0 16 16" width="18" height="18" fill="none"><path d="M14 11 C14 3 2 3 2 11" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"/><path d="M4.5 8.5 L2 11 L4.5 13.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><circle cx="8" cy="14" r="1.3" fill="currentColor"/></svg>
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip title="Reverse Step Into">
						<span>
							<IconButton size="small" onClick={this.reverseStepInto} disabled={!hasFrame}>
								<svg viewBox="0 0 16 16" width="18" height="18" fill="none"><path d="M13 3 H8 V10" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M5.5 7.5 L8 10 L10.5 7.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M3 14 H13" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"/></svg>
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip title="Reverse Step Out">
						<span>
							<IconButton size="small" onClick={this.reverseStepOut} disabled={!hasFrame}>
								<svg viewBox="0 0 16 16" width="18" height="18" fill="none"><path d="M8 12 V5 H3" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M5.5 2.5 L3 5 L5.5 7.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/><path d="M3 14 H13" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"/></svg>
							</IconButton>
						</span>
					</Tooltip>
				</Box>
				<Box sx={{ flexGrow: 1, minHeight: 0 }}>
				<CustomSplit>
					{/* ========== LEFT COLUMN: Stack + Aux ========== */}
					<Box
						sx={{
							width: "25%",
							minWidth: 200,
							height: "100%",
						}}
					>
						<CustomSplit mode="vertical">
							{/* Stack Trace with thread tabs */}
							<Box
								sx={{
									height: "55%",
									minHeight: 100,
									display: "flex",
									flexDirection: "column",
								}}
							>
								<Box
									display="flex"
									alignItems="center"
									gap={0.5}
								>
									<Tabs
										value={
											threadIndex >= 0
												? threadIndex
												: 0
										}
										onChange={this.threadTabChanged}
										variant="scrollable"
										scrollButtons="auto"
										sx={{
											minHeight: 28,
											flexGrow: 1,
											"& .MuiTab-root": {
												minHeight: 28,
												py: 0,
												fontSize:
													"0.72rem",
											},
										}}
									>
										{debuggers.map((d, i) => (
											<Tab
												key={d.id}
												label={
													d.label ||
													d.description ||
													"Thread " +
														(i + 1)
												}
											/>
										))}
									</Tabs>
									<Tooltip title="Refresh threads">
										<IconButton
											size="small"
											onClick={
												this.loadDebuggers
											}
										>
											<RefreshIcon fontSize="small" />
										</IconButton>
									</Tooltip>
								</Box>
								<Box
									sx={{
										flexGrow: 1,
										overflow: "auto",
									}}
								>
									<StackTracePanel
										frames={frames}
										selectedFrame={selectedFrame}
										onFrameSelect={(f) =>
											this.selectFrame(f)
										}
									/>
								</Box>
							</Box>
							{/* Auxiliary tabs */}
							<Box
								sx={{
									height: "45%",
									minHeight: 0,
									display: "flex",
									flexDirection: "column",
									overflow: "hidden",
								}}
							>
								<Tabs
									value={auxTab}
									onChange={(e, v) =>
										this.setState({ auxTab: v })
									}
									variant="scrollable"
									scrollButtons="auto"
									sx={{
										minHeight: 26,
										flexShrink: 0,
										"& .MuiTab-root": {
											minHeight: 26,
											py: 0,
											px: 1,
											fontSize: "0.7rem",
											minWidth: 0,
										},
									}}
								>
									<Tab label="Modules" />
									<Tab label="Symbols" />
									<Tab label="Regions" />
									<Tab label="Classify" />
									<Tab label="Memory" />
									<Tab label="Resolver" />
								</Tabs>
								<Box
									sx={{
										flexGrow: 1,
										minHeight: 0,
										overflow: "hidden",
										p: 0.5,
									}}
								>
									{auxTab === 0 && <ModulesPanel />}
									{auxTab === 1 && (
										<SymbolsPanel
											onInspectExpression={
												this
													.handleInspectExpression
											}
										/>
									)}
									{auxTab === 2 && (
										<MemoryRegionsPanel
											regions={regions}
											onAddressClick={
												this
													.handleAddressClick
											}
										/>
									)}
									{auxTab === 3 && (
										<AddressClassifierPanel />
									)}
									{auxTab === 4 && (
										<MemoryViewerPanel />
									)}
									{auxTab === 5 && (
										<SymbolResolverPanel
											onAddressClick={
												this
													.handleAddressClick
											}
										/>
									)}
								</Box>
							</Box>
						</CustomSplit>
					</Box>
					{/* ========== CENTER COLUMN: Code + Console ========== */}
					<Box
						sx={{
							width: "50%",
							minWidth: 280,
							height: "100%",
						}}
					>
						<CustomSplit mode="vertical">
							{/* Code */}
							<Box
								sx={{
									height: "70%",
									minHeight: 100,
									display: "flex",
									flexDirection: "column",
								}}
							>
								<PanelHeader title="Code">
									<ToggleButtonGroup
										size="small"
										exclusive
										value={codeMode}
										onChange={(e, v) => {
											if (v)
												this.setState({
													codeMode: v,
												});
										}}
										sx={{ height: 22 }}
									>
										<ToggleButton
											value="high-level"
											sx={{
												fontSize: "0.65rem",
												px: 1,
											}}
										>
											High-level
										</ToggleButton>
										<ToggleButton
											value="asm"
											sx={{
												fontSize: "0.65rem",
												px: 1,
											}}
										>
											ASM
										</ToggleButton>
									</ToggleButtonGroup>
								</PanelHeader>
								<Box
									sx={{
										flexGrow: 1,
										overflow: "auto",
										p: 0.5,
									}}
								>
									{this.renderCodePanel()}
								</Box>
							</Box>
							{/* Debug Console */}
							<Box
								sx={{
									height: "30%",
									minHeight: 60,
									display: "flex",
									flexDirection: "column",
								}}
							>
								<PanelHeader title="Console" />
								<Box
									sx={{
										flexGrow: 1,
										overflow: "auto",
										p: 0.5,
										fontFamily: monoFont,
										fontSize: "0.8rem",
										backgroundColor:
											"action.hover",
									}}
								>
									{consoleHistory.map(
										(entry, i) => (
											<Box key={i} mb={0.5}>
												<Typography
													component="span"
													sx={{
														fontFamily:
															"monospace",
														fontSize:
															"0.8rem",
														color: "info.main",
													}}
												>
													{"> "}
													{entry.input}
												</Typography>
												<br />
												<Typography
													component="span"
													sx={{
														fontFamily:
															"monospace",
														fontSize:
															"0.8rem",
														color: entry.error
															? "error.main"
															: "text.primary",
													}}
												>
													{entry.error ||
														entry.output}
												</Typography>
											</Box>
										)
									)}
								</Box>
								<Box
									display="flex"
									alignItems="center"
									sx={{
										borderTop: 1,
										borderColor: "divider",
									}}
								>
									<TextField
										size="small"
										fullWidth
										placeholder="expression..."
										value={consoleInput}
										onChange={(e) =>
											this.setState({
												consoleInput:
													e.target.value,
											})
										}
										onKeyDown={(e) => {
											if (e.key === "Enter")
												this.consoleEval();
										}}
										variant="standard"
										InputProps={{
											disableUnderline: true,
											sx: {
												fontFamily:
													"monospace",
												fontSize: "0.8rem",
												px: 0.5,
											},
										}}
									/>
								</Box>
							</Box>
						</CustomSplit>
					</Box>
					{/* ========== RIGHT COLUMN: Context + Focus ========== */}
					<Box
						sx={{
							width: "25%",
							minWidth: 200,
							height: "100%",
						}}
					>
						<CustomSplit mode="vertical">
							{/* Inspection Context: frame bindings */}
							<Box
								sx={{
									height: "50%",
									minHeight: 80,
									display: "flex",
									flexDirection: "column",
								}}
							>
								<PanelHeader title="Inspection Context" />
								<Box
									sx={{
										flexGrow: 1,
										overflow: "auto",
										p: 0.5,
									}}
								>
									{bindings.length > 0 ? (
										<TableContainer>
											<Table
												size="small"
												stickyHeader
												sx={{ '& .MuiTableCell-root': { borderBottom: 'none' } }}
											>
												<TableHead>
													<TableRow>
														<TableCell sx={{ py: 0.25 }}>
															<strong>
																Name
															</strong>
														</TableCell>
														<TableCell sx={{ py: 0.25 }}>
															<strong>
																Value
															</strong>
														</TableCell>
														<TableCell sx={{ py: 0.25 }}>
															<strong>
																Type
															</strong>
														</TableCell>
													</TableRow>
												</TableHead>
												<TableBody>
													{bindings.map(
														(b, i) => {
															const nameCell = (
																<TableCell
																	sx={{
																		fontFamily:
																			"monospace",
																		fontSize:
																			"0.78rem",
																		py: 0.15,
																	}}
																>
																	{
																		b.name
																	}
																</TableCell>
															);
															return (
															<TableRow
																key={
																	i
																}
																hover
																sx={{
																	cursor: "pointer",
																}}
																onClick={() => {
																	const addr = b.oop || b.value;
																	if (!addr) return;
																	const val = typeof addr === "string" ? addr : "";
																	if (b.oop || (val.startsWith && (val.startsWith("0x") || val.startsWith("0X")))) {
																		this.handleAddressClick(
																			addr,
																			b.oop ? null : b.type
																		);
																	}
																}}
															>
																{b.location ? (
																	<Tooltip title={b.location} placement="left">
																		{nameCell}
																	</Tooltip>
																) : nameCell}
																<TableCell
																	sx={{
																		fontFamily:
																			"monospace",
																		fontSize:
																			"0.78rem",
																		py: 0.15,
																	}}
																>
																	{b.value ||
																		JSON.stringify(
																			b
																		)}
																</TableCell>
																<TableCell
																	sx={{
																		fontSize:
																			"0.7rem",
																		py: 0.15,
																		color: "text.secondary",
																	}}
																>
																	{b.type || ""}
																</TableCell>
															</TableRow>
														);
														}
													)}
												</TableBody>
											</Table>
										</TableContainer>
									) : (
										<Typography
											variant="body2"
											color="text.secondary"
										>
											(select a frame to see
											bindings)
										</Typography>
									)}
								</Box>
							</Box>
							{/* Inspection Focus: OOP / address detail */}
							<Box
								sx={{
									height: "50%",
									minHeight: 80,
									display: "flex",
									flexDirection: "column",
								}}
							>
								<PanelHeader title="Inspection Focus" />
								<Box
									sx={{
										flexGrow: 1,
										overflow: "auto",
										p: 0.5,
									}}
								>
									<OopInspectorPanel
										initialOop={inspectOop}
										typeHint={inspectType}
										version={inspectVersion}
									/>
								</Box>
							</Box>
						</CustomSplit>
					</Box>
				</CustomSplit>
				</Box>
			</Box>
		);
	}
}

export default VMInspector;
