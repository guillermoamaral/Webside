import React, { Component } from "react";
import { Grid, Paper } from "@material-ui/core";
import CustomTable from "../controls/CustomTable";
import Inspector from "./Inspector";
import { ide } from "../IDE";
import clsx from "clsx";

class ObjectBrowser extends Component {
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
			await ide.api.unpinObject(object.id);
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
		const styles = this.props.styles;
		const ow = selectedObject ? 8 : 12;
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		return (
			<Grid container spacing={1}>
				<Grid item xs={ow} md={ow} lg={ow}>
					<Paper className={fixedHeightPaper} variant="outlined">
						<CustomTable
							styles={styles}
							columns={this.objectColumns()}
							rows={rows}
							onSelect={this.objectSelected}
							menuOptions={this.menuOptions()}
						/>
					</Paper>
				</Grid>
				{selectedObject && (
					<Grid item xs={4} md={4} lg={4}>
						<Paper variant="outlined">
							<Inspector
								styles={styles}
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
