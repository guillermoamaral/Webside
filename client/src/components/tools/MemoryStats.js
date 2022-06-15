import React, { Component } from "react";
import { IDEContext } from "../IDEContext";
import { Grid } from "@material-ui/core";
import { Line } from "react-chartjs-2";

class MemoryStats extends Component {
	static contextType = IDEContext;
	
	constructor(props) {
		super(props);
		this.state = {
			loading: true,
			stats: [],
		};
	}

	componentDidMount() {
		this.setTimer();
	}

	componentWillUnmount() {
		clearTimeout(this.timer);
	}

	setTimer() {
		this.timer = setTimeout(() => {
			this.updateStats();
		}, 1000);
	}

	async updateStats() {
		try {
			const stats = await this.context.api.getMemoryStats(100);
			this.setState({ stats: stats, loading: false });
			this.setTimer();
		} catch (error) {
			this.context.reportError(error);
		}
	}

	render() {
		const { stats } = this.state;
		const sizesObptions = {
			title: {
				display: true,
				text: "Space Size",
			},
			legend: {
				position: "bottom",
			},
			scales: {
				yAxes: [
					{
						type: "linear",
						display: true,
						position: "left",
						id: "size",
						scaleLabel: {
							display: true,
							labelString: "kb",
						},
					},
					{
						type: "linear",
						display: true,
						position: "right",
						id: "percent",
						scaleLabel: {
							display: true,
							labelString: "%",
						},
					},
				],
			},
			elements: {
				line: {
					tension: 0.1,
				},
			},
			animation: {
				duration: 0,
			},
		};
		const sizes = {
			labels: stats.map((s, i) => i.toString()),
			datasets: [
				{
					label: "Old Space Size (kb)",
					data: stats.map((s) => s.oldSize / 1024),
					fill: false,
					borderColor: "rgb(68, 212, 146)",
					yAxisID: "size",
					borderWidth: 1,
					pointRadius: 0,
				},
				{
					label: "Young Space Size (kb)",
					data: stats.map((s) => s.youngSize / 1024),
					fill: false,
					borderColor: "rgb(245, 235, 103)",
					yAxisID: "size",
					borderWidth: 1,
					pointRadius: 0,
				},
				{
					label: "Cradled Size (kb)",
					data: stats.map((s) => s.cradled / 1024),
					fill: false,
					borderColor: "rgb(255, 161, 92)",
					yAxisID: "size",
					borderWidth: 1,
					pointRadius: 0,
				},
				{
					label: "Tenured Size (kb)",
					data: stats.map((s) => s.tenured / 1024),
					fill: false,
					borderColor: "rgb(250, 35, 62)",
					yAxisID: "size",
					borderWidth: 1,
					pointRadius: 0,
				},
				{
					label: "Survival Rate (%)",
					data: stats.map((s) => s.survivalRate * 100),
					fill: false,
					borderColor: "rgb(191, 38, 105)",
					yAxisID: "percent",
					borderWidth: 1,
					pointRadius: 0,
				},
			],
		};
		const numbersOptions = {
			title: {
				display: true,
				text: "Objects",
			},
			legend: {
				position: "bottom",
			},
			scales: {
				yAxes: [
					{
						type: "linear",
						display: true,
						position: "left",
						id: "number",
						scaleLabel: {
							display: true,
							labelString: "# of objects",
						},
					},
				],
			},
			animation: {
				duration: 0,
			},
		};
		const numbers = {
			labels: stats.map((s, i) => i.toString()),
			datasets: [
				{
					label: "Remembered Before GC",
					data: stats.map((s) => s.rememberedBefore),
					fill: false,
					borderColor: "rgb(112, 42, 140)",
					yAxisID: "number",
					borderWidth: 1,
					pointRadius: 0,
				},
				{
					label: "Remembered After GC",
					data: stats.map((s) => s.rememberedAfter),
					fill: false,
					borderColor: "rgb(255, 115, 38)",
					yAxisID: "number",
					borderWidth: 1,
					pointRadius: 0,
				},
				{
					label: "Rescued",
					data: stats.map((s) => s.rescued),
					fill: false,
					borderColor: "rgb(255, 115, 38)",
					yAxisID: "number",
					borderWidth: 1,
					pointRadius: 0,
				},
			],
		};
		const timesOptions = {
			title: {
				display: true,
				text: "Time",
			},
			legend: {
				position: "bottom",
			},
			scales: {
				yAxes: [
					{
						type: "linear",
						display: true,
						position: "left",
						id: "time",
						scaleLabel: {
							display: true,
							labelString: "ms",
						},
					},
				],
			},
			animation: {
				duration: 0,
			},
		};
		const times = {
			labels: stats.map((s, i) => i.toString()),
			datasets: [
				{
					label: "Time (ms)",
					data: stats.map((s) => s.time),
					fill: false,
					borderColor: "rgb(112, 42, 140)",
					yAxisID: "time",
					borderWidth: 1,
					pointRadius: 0,
				},
			],
		};
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Line height={80} data={sizes} options={sizesObptions} />
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Line height={80} data={numbers} options={numbersOptions} />
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Line height={80} data={times} options={timesOptions} />
				</Grid>
			</Grid>
		);
	}
}

export default MemoryStats;
