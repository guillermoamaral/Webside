import React, { Component } from "react";
import { Button, TextField, Grid, FormHelperText } from "@mui/material";
import axios from "axios";
import { withNavigation } from "./withNavigation";

class Login extends Component {
	constructor(props) {
		super(props);
		this.state = {
			baseUri: "",
			developer: "",
			dialect: "",
			connecting: false,
			error: null,
		};
	}

	baseUriChanged(uri) {
		this.setState({ baseUri: uri, error: null });
	}

	developerChanged(developer) {
		this.setState({ developer: developer });
	}

	connectClicked = async (event) => {
		event.preventDefault();
		const { baseUri, developer } = this.state;
		if (baseUri && baseUri !== "" && developer && developer !== "") {
			try {
				this.setState({ connecting: true });
				const response = await axios.get(baseUri + "/dialect");
				const dialect = response.data;
				this.setState({ dialect: dialect, connecting: false });
				this.props.navigate(
					"/ide?baseUri=" +
						baseUri +
						"&dialect=" +
						dialect +
						"&developer=" +
						developer
				);
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
		const { baseUri, developer, error, connecting } = this.state;
		const buttonLabel = connecting ? "Connecting" : "Connect";
		return (
			<div
				sx={{
					display: "flex",
				}}
			>
				<Grid
					container
					direction="column"
					justifyContent="center"
					alignContent="center"
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
						<form onSubmit={this.connectClicked}>
							<Grid
								container
								direction="column"
								spacing={1}
								alignItems="flex-end"
							>
								<Grid item>
									<TextField
										size="small"
										id="baseUri"
										label="Target Smalltalk URL"
										type="url"
										placeholder="URL"
										margin="dense"
										fullWidth
										name="baseUri"
										variant="outlined"
										value={baseUri || ""}
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
										size="small"
										id="developer"
										label="Developer"
										type="text"
										placeholder="developer"
										margin="dense"
										fullWidth
										name="developer"
										variant="outlined"
										value={developer || ""}
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
										<FormHelperText>{error}</FormHelperText>
									</Grid>
								)}
							</Grid>
						</form>
					</Grid>
				</Grid>
			</div>
		);
	}
}

export default withNavigation(Login);
