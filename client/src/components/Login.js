import React, { Component } from "react";
import { Grid } from "@material-ui/core";
import ConnectionSettings from "./ConnectionSettings";
import { withRouter } from "react-router-dom";

class Login extends Component {
	connectClicked = (settings) => {
		this.props.history.push(
			"/ide?baseUri=" +
				settings.baseUri +
				"&dialect=" +
				settings.dialect +
				"&developer=" +
				settings.developer
		);
	};

	render() {
		return (
			<div className={this.props.styles.root}>
				<Grid
					container
					direction="column"
					justify="center"
					alignItems="center"
					spacing={0}
					style={{ minHeight: "80vh" }}
				>
					<Grid item>
						<img
							alt="Webside"
							src={require("../resources/webSide.png")}
							width={200}
							height={100}
						/>
					</Grid>
					<Grid item>
						<ConnectionSettings
							styles={this.props.styles}
							acceptLabel="Connect"
							onAccept={this.connectClicked}
						/>
					</Grid>
				</Grid>
			</div>
		);
	}
}

export default withRouter(Login);
