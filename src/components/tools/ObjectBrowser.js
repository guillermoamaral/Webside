import React from "react";
import Tool from "./Tool";
import { Grid, Paper } from "@mui/material";
import CustomTable from "../controls/CustomTable";
import Inspector from "./Inspector";
import { ide } from "../IDE";
import CustomPaper from "../controls/CustomPaper";

class ObjectBrowser extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			selectedObject: null,
		};
	}

	objectSelected = (object) => {
		this.setState({ selectedObject: object });
	};

	unpinObject = async (object) => {
		try {
			await ide.backend.unpinObject(object.id);
		} catch (error) {
			ide.reportError(error);
		}
	};

	menuOptions() {
		return [{ label: "Unpin", action: this.unpinObject }];
	}

	objectColums() {
		return [
			{ field: "id", label: "ID", align: "left" },
			{
				field: "printString",
				label: "Print String",
				minWidth: 200,
				align: "left",
			},
		];
	}

	render() {
		const selectedObject = this.state.selectedObject;
		const rows = this.props.objects;
		const ow = selectedObject ? 8 : 12;
		return (
			<Grid container spacing={1}>
				<Grid item xs={ow} md={ow} lg={ow}>
					<CustomPaper>
						<CustomTable
							columns={this.objectColumns()}
							rows={rows}
							onRowSelect={this.objectSelected}
							menuOptions={this.menuOptions()}
						/>
					</CustomPaper>
				</Grid>
				{selectedObject && (
					<Grid item xs={4} md={4} lg={4}>
						<Paper variant="outlined">
							<Inspector
								root={selectedObject}
								showWorkspace={false}
							/>
						</Paper>
					</Grid>
				)}
			</Grid>
		);
	}
}

export default ObjectBrowser;
