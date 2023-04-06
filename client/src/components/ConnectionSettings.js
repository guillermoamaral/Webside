import React, { Component } from "react";
import { Button, TextField, Grid, FormHelperText } from "@material-ui/core";
import axios from "axios";

class ConnectionSettings extends Component {
	constructor(props) {
		super(props);
		const defaultSettings = { baseUri: "", developer: "" };
		this.state = {
			settings: this.props.settings || defaultSettings,
			connecting: false,
			error: null,
		};
	}

	baseUriChanged(uri) {
		const settings = this.state.settings;
		settings.baseUri = uri;
		this.setState({ settings: settings, error: null });
	}

	developerChanged(developer) {
		const settings = this.state.settings;
		settings.developer = developer;
		this.setState({ settings: settings });
	}

	acceptClicked = async (event) => {
		event.preventDefault();
		const { settings } = this.state;
		if (
			settings.baseUri &&
			settings.baseUri !== "" &&
			settings.developer &&
			settings.developer !== ""
		) {
			try {
				this.setState({ connecting: true });
				const response = await axios.get(settings.baseUri + "/dialect");
				settings.dialect = response.data;
				this.setState({ connecting: false });
				if (this.props.onAccept) {
					this.props.onAccept(settings);
				}
			} catch (error) {
				this.setState({
					error: "Cannot connect to target Smalltalk",
					connecting: false,
				});
			}
		} else {
			this.setState({
				error: "You must complete the fields",
				connecting: false,
			});
		}
	};

	render() {
		const { settings, error, connecting } = this.state;
		const buttonLabel = connecting
			? "Connecting"
			: this.props.acceptLabel || "Accept";
		return (
			<div className={this.props.styles.root}>
				<Grid container direction="column" justify="center" spacing={1}>
					<Grid item>
						<Grid
							container
							direction="row"
							justify="center"
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
												disabled={connecting}
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
												disabled={connecting}
											/>
										</Grid>
										<Grid item>
											<Button
												variant="outlined"
												type="submit"
												disabled={connecting}
											>
												{buttonLabel}
											</Button>
										</Grid>
										{error && (
											<Grid item>
												<FormHelperText>
													{error}
												</FormHelperText>
											</Grid>
										)}
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
