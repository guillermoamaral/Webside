import React, { Component } from "react";
import { Button, TextField, Grid } from "@material-ui/core";
import axios from "axios";

class ConnectionSettings extends Component {
	constructor(props) {
		super(props);
		const defaultSettings = { baseUri: "", developer: "" };
		this.state = {
			settings: this.props.settings || defaultSettings,
		};
	}

	baseUriChanged(uri) {
		const settings = this.state.settings;
		settings.baseUri = uri;
		this.setState(settings);
	}

	developerChanged(developer) {
		const settings = this.state.settings;
		settings.developer = developer;
		this.setState(settings);
	}

	acceptClicked = async (event) => {
		event.preventDefault();
		const settings = this.state;
		if (
			settings.baseUri &&
			settings.baseUri !== "" &&
			settings.developer &&
			settings.developer !== ""
		) {
			const response = await axios.get(settings.baseUri + "/dialect");
			settings.dialect = response.data;
			if (this.props.onAccept) {
				this.props.onAccept(settings);
			}
		} else {
			alert("You must complete the fields");
		}
	};

	render() {
		const settings = this.state.settings;
		return (
			<div className={this.props.styles.root}>
				<Grid
					container
					direction="column"
					justifyContent="center"
					spacing={1}
				>
					<Grid item>
						<Grid
							container
							direction="row"
							justifyContent="center"
							spacing={1}
						>
							<Grid item>
								<form onSubmit={this.acceptClicked}>
									<Grid
										container
										direction="column"
										spacing={1}
										alignItems="flex-end"
									>
										<Grid item>
											<TextField
												id="baseUri"
												label="Target Smalltalk URL"
												type="url"
												placeholder="URL"
												margin="dense"
												fullWidth
												name="baseUri"
												variant="outlined"
												value={settings.baseUri || ""}
												onChange={(event) =>
													this.baseUriChanged(
														event.target.value
													)
												}
												required
												autoFocus
											/>
										</Grid>
										<Grid item>
											<TextField
												id="developer"
												label="Developer"
												type="text"
												placeholder="developer"
												margin="dense"
												fullWidth
												name="developer"
												variant="outlined"
												value={settings.developer || ""}
												onChange={(event) =>
													this.developerChanged(
														event.target.value
													)
												}
												required
											/>
										</Grid>
										<Grid item>
											<Button
												variant="outlined"
												type="submit"
											>
												{this.props.acceptLabel ||
													"Accept"}
											</Button>
										</Grid>
									</Grid>
								</form>
							</Grid>
						</Grid>
					</Grid>
				</Grid>
			</div>
		);
	}
}

export default ConnectionSettings;
