import React, { Component } from "react";
import { Button, TextField, Grid } from "@material-ui/core";
import axios from "axios";

class Settings extends Component {
	constructor(props) {
		super(props);
		this.state = {
			baseUri: this.props.baseUri || "",
			developer: this.props.developer || "",
		};
	}

	acceptClicked = async (event) => {
		event.preventDefault();
		const { baseUri, developer } = this.state;
		if (baseUri && baseUri !== "" && developer && developer !== "") {
			const response = await axios.get(baseUri + "/dialect");
			if (this.props.onAccept) {
				this.props.onAccept(baseUri, response.data, developer);
			}
		} else {
			alert("You must complete the fields");
		}
	};

	render() {
		const { baseUri, developer } = this.state;
		return (
			<div className={this.props.styles.root}>
				<Grid container direction="column" justify="center" spacing={1}>
					<Grid item>
						<Grid container direction="row" justify="center" spacing={1}>
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
												value={baseUri}
												onChange={(event) =>
													this.setState({ baseUri: event.target.value })
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
												value={developer}
												onChange={(event) =>
													this.setState({ developer: event.target.value })
												}
												required
											/>
										</Grid>
										<Grid item>
											<Button variant="outlined" type="submit">
												{this.props.acceptLabel || "Accept"}
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

export default Settings;
