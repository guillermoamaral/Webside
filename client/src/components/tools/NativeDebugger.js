import React, { Component } from "react";
import { Grid, Paper, IconButton, Tooltip } from "@material-ui/core";
import clsx from "clsx";
import { Icon } from "@iconify/react";
import SuspendIcon from "@iconify/icons-mdi/pause";
import ResumeIcon from "@iconify/icons-mdi/play";
import TerminateIcon from "@iconify/icons-mdi/stop";
import { IDEContext } from "../IDEContext";
import FrameList from "../parts/FrameList";
import RegisterTable from "../parts/RegisterTable";
import CodeBrowser from "../parts/CodeBrowser";
import { HorizontalBar } from "react-chartjs-2";

class NativeDebugger extends Component {
	static contextType = IDEContext;
	
	constructor(props) {
		super(props);
		this.state = {
			running: false,
			frames: [],
			selectedFrame: null,
			registers: [],
			spaces: [],
		};
	}

	componentDidMount() {
		this.updateInfo();
	}

	async updateInfo() {
		try {
			const native = await this.context.api.getNativeDebugger(this.props.id);
			const running = native.state === "running";
			const frames = await this.context.api.getNativeDebuggerFrames(
				this.props.id
			);
			let selected = null;
			if (frames.length > 0) {
				selected = frames[0];
				await this.updateFrame(selected);
			}
			const registers = await this.context.api.getNativeDebuggerRegisters(
				this.props.id
			);
			const spaces = await this.context.api.getNativeDebuggerSpaces(
				this.props.id
			);
			spaces.forEach((s) => (s.color = this.colorForSpace(s)));
			this.setState({
				running: running,
				frames: frames,
				selectedFrame: selected,
				registers: registers,
				spaces: spaces,
			});
		} catch (error) {
			this.context.reportError(error);
		}
	}

	resumeClicked = async () => {
		try {
			await this.context.api.resumeNativeDebugger(this.props.id);
			this.setState({ running: true });
			this.updateInfo();
		} catch (error) {
			this.context.reportError(error);
		}
	};

	suspendClicked = async () => {
		try {
			await this.context.api.suspendNativeDebugger(this.props.id);
			this.setState({ running: false });
		} catch (error) {
			this.context.reportError(error);
		}
	};

	frameSelected = async (frame) => {
		await this.updateFrame(frame);
		this.setState({ selectedFrame: frame });
	};

	updateFrame = async (frame) => {
		if (!frame.method) {
			try {
				const info = await this.context.api.getNativeDebuggerFrame(
					this.props.id,
					frame.index
				);
				frame.method = info.method;
				frame.class = info.class;
				frame.interval = info.interval;
			} catch (error) {
				this.context.reportError(error);
			}
		}
	};

	spaceConaining(address) {
		return this.state.spaces.find(
			(s) => s.base <= address && address <= s.commitedLimit
		);
	}

	colorForSpace(space) {
		var color;
		switch (space.name) {
			case "Kernel":
				color = "rgb(171, 233, 103)";
				break;
			case "Eden":
				color = "rgb(60, 232, 240)";
				break;
			case "From":
				color = "rgb(67, 204, 233)";
				break;
			case "To":
				color = "rgb(49, 150, 251)";
				break;
			case "Pinned Object":
				color = "rgb(224, 63, 237)";
				break;
			default:
				color = "rgb(242, 205, 57)";
		}
		return color;
	}

	randomColor() {
		var letters = "0123456789ABCDEF".split("");
		var color = "#";
		for (var i = 0; i < 6; i++) {
			color += letters[Math.round(Math.random() * 15)];
		}
		return color;
	}

	render() {
		const styles = this.props.styles;
		const { running, frames, selectedFrame, registers, spaces } = this.state;
		registers.forEach((r) => {
			const s = this.spaceConaining(r.value);
			r.color = s ? s.color : null;
		});
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		var spacesData = { labels: ["address"], datasets: [] };
		var offset;
		spaces.forEach((s) => {
			if (offset && s.base > offset) {
				spacesData.datasets.push({
					label: "empty space from " + offset + " to " + s.base,
					data: [s.base - offset],
					backgroundColor: "rgba(255, 255, 255, 0)",
				});
			}
			spacesData.datasets.push({
				label: s.name,
				data: [s.commitedLimit - s.base],
				backgroundColor: s.color,
			});
			offset = s.commitedLimit;
		});
		const min = spaces.length > 0 ? spaces[0].base : 0;
		const max = spaces.length > 0 ? spaces[spaces.length - 1].commitedLimit : 1;
		const spacesOptions = {
			legend: { display: false },
			scales: {
				yAxes: [{ stacked: true, display: false }],
				xAxes: [
					{
						stacked: true,
						display: true,
						suggestedMin: min,
						suggestedMax: max,
					},
				],
			},
		};
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<HorizontalBar
						height={16}
						data={spacesData}
						options={spacesOptions}
					/>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{/* <Tooltip title="Step into" placement="top">
                        <IconButton color="inherit" onClick={this.stepIntoClicked} size="medium">
                            <Icon icon={StepIntoIcon}/>
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Step over" placement="top">
                        <IconButton color="inherit" onClick={this.stepOverClicked} size="medium">
                            <Icon icon={StepOverIcon}/>
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Restart" placement="top">
                        <IconButton color="inherit" onClick={this.restartClicked} size="medium">
                            <Icon icon={RestartIcon}/>
                        </IconButton>
                    </Tooltip> */}
					<Tooltip title={running ? "Suspend" : "Resume"} placement="top">
						<IconButton
							color="inherit"
							onClick={running ? this.suspendClicked : this.resumeClicked}
							size="medium"
						>
							<Icon icon={running ? SuspendIcon : ResumeIcon} />
						</IconButton>
					</Tooltip>
					<Tooltip title="Terminate" placement="top">
						<IconButton
							color="inherit"
							onClick={this.terminateClicked}
							size="medium"
						>
							<Icon icon={TerminateIcon} />
						</IconButton>
					</Tooltip>
				</Grid>
				<Grid item xs={7} md={7} lg={7}>
					<CodeBrowser
						context={{
							debugger: this.props.id,
							frame: selectedFrame ? selectedFrame.index : null,
						}}
						styles={styles}
						class={selectedFrame ? selectedFrame.class : null}
						method={selectedFrame ? selectedFrame.method : null}
						selectedInterval={selectedFrame ? selectedFrame.interval : null}
					/>
				</Grid>
				<Grid item xs={5} md={5} lg={5}>
					<Grid container spacing={1}>
						<Grid item xs={12} md={12} lg={12}>
							<Paper className={fixedHeightPaper} variant="outlined">
								<RegisterTable
									debugger={this.props.id}
									styles={styles}
									registers={registers}
								/>
							</Paper>
						</Grid>
						<Grid item xs={12} md={12} lg={12}>
							<Paper className={fixedHeightPaper} variant="outlined">
								<FrameList
									frames={frames}
									selectedFrame={selectedFrame}
									onSelect={this.frameSelected}
								/>
							</Paper>
						</Grid>
					</Grid>
				</Grid>
			</Grid>
		);
	}
}

export default NativeDebugger;
