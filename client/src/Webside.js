import React, { Component } from "react";
import { BrowserRouter as Router, Switch, Route } from "react-router-dom";
import { withStyles } from "@material-ui/core/styles";
import styles from "./styles";
import { DialogProvider } from "./components/dialogs/index";
import { CookiesProvider } from "react-cookie";
import Login from "./components/Login";
import IDE from "./components/IDE";

class Webside extends Component {
	render() {
		return (
			<DialogProvider>
				<CookiesProvider>
					<div className={styles.root}>
						<Router>
							<Switch>
								<Route
									path="/"
									exact
									component={() => (
										<Login styles={this.props.classes} />
									)}
								/>
								<Route
									path="/ide/"
									exact
									component={() => (
										<IDE styles={this.props.classes} />
									)}
								/>
								<Route
									path="/ide?baseUri=:baseUri"
									exact
									component={() => (
										<IDE styles={this.props.classes} />
									)}
								/>
								<Route
									path="/ide/classes/:classname"
									exact
									component={() => (
										<IDE styles={this.props.classes} />
									)}
								/>
								<Route
									path="/ide/debuggers/:debugger"
									exact
									component={() => (
										<IDE styles={this.props.classes} />
									)}
								/>
							</Switch>
						</Router>
					</div>
				</CookiesProvider>
			</DialogProvider>
		);
	}
}

export default withStyles(styles)(Webside);
