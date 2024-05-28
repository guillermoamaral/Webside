import React from "react";
import Tool from "./Tool";
import { Box } from "@mui/material";
import { LineChart } from "@mui/x-charts/LineChart";
import { mangoFusionPalette } from "@mui/x-charts/colorPalettes";
import { axisClasses } from "@mui/x-charts/ChartsAxis";

class SystemStats extends Tool {
	groupedStats() {
		const groups = {};
		this.props.stats.forEach((stat) => {
			let group = groups[stat.unit];
			if (!group) {
				group = { unit: stat.unit, series: [] };
				groups[stat.unit] = group;
			}
			let series = group.series.find((s) => s.label === stat.label);
			if (!series) {
				series = { label: stat.label, data: [] };
				group.series.push(series);
			}
			series.data.push(stat.value);
		});
		return Object.values(groups);
	}

	render() {
		const groups = this.groupedStats();
		const rows = [];
		for (let r = 0; r < Math.floor(groups.length / 2) + 1; r++)
			rows.push(r);
		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				{rows.map((r) => {
					return (
						<Box display="flex" flexDirection="row" key={"row" + r}>
							{[0, 1]
								.filter((c) => groups[r * 2 + c])
								.map((c) => {
									const group = groups[r * 2 + c];
									return (
										<Box key={group.unit} flexGrow={1}>
											<LineChart
												margin={{
													left: 100,
												}}
												xAxis={[
													{
														data: group.series[0].data.map(
															(d, i) => i
														),
													},
												]}
												yAxis={[
													{
														label:
															group.unit !==
															"none"
																? group.unit
																: "",
													},
												]}
												series={group.series.map(
													(s) => {
														return {
															data: s.data,
															label: s.label,
															showMark: false,
															valueFormatter: (
																v
															) =>
																parseFloat(
																	v.toFixed(2)
																) +
																" (" +
																group.unit +
																")",
														};
													}
												)}
												colors={mangoFusionPalette}
												//width={500}
												height={300}
												slotProps={{
													legend: {
														direction: "row",
														position: {
															vertical: "top",
															horizontal:
																"middle",
														},
														padding: -5,
													},
												}}
												sx={{
													[`.${axisClasses.left} .${axisClasses.label}`]:
														{
															// Move the y-axis label with CSS
															transform:
																"translateX(-30px)",
														},
												}}
											/>
										</Box>
									);
								})}
						</Box>
					);
				})}
			</Box>
		);
	}
}

export default SystemStats;
