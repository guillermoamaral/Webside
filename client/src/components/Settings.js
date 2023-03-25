import React, { Component } from "react";
import { Grid, Divider, Typography } from "@material-ui/core";
import ConnectionSettings from "./ConnectionSettings";
import { withRouter } from "react-router-dom";

class Settings extends Component {
	applyConnectionSettings = (settings) => {
		const handler = this.props.onApplyConnectionSettings;
		if (handler) {
			handler(settings);
		}
	};

	render() {
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Divider orientation="horizontal" />
					<Typography variant="h6">Connection</Typography>
					<ConnectionSettings
						styles={this.props.styles}
						settings={this.props.settings}
						acceptLabel="Apply"
						onAccept={this.applyConnectionSettings}
					/>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Divider orientation="horizontal" />
					<Typography variant="h6">Appearance</Typography>
				</Grid>
			</Grid>
		);
	}
}

export default withRouter(Settings);
