import React, { Component } from "react";
import { ThemeProvider, createTheme } from "@mui/material/styles";
import CssBaseline from "@mui/material/CssBaseline";
import Login from "./components/Login";
import IDE from "./components/IDE";
import { HashRouter as Router, Routes, Route } from "react-router-dom";
import { DialogProvider } from "./components/dialogs/index";

var app = null;

class App extends Component {
	constructor(props) {
		super(props);
		app = this;
		this.theme = createTheme({
			palette: {
				mode: "dark",
				primary: {
					main: "#00000",
				},
				text: {
					primary: "#aaaaaa",
					secondary: "#00000",
				},
				background: {
					main: "#303030",
					default: "#303030",
				},
			},
		});
	}

	updateTheme(settings) {
		const appearance = settings.section("appearance");
		const mode = appearance.get("mode");
		const colors = appearance.section(mode);
		const background = colors.get("background");
		this.theme = createTheme({
			typography: {
				fontFamily: appearance.get("fontFamily"),
				fontSize: appearance.get("fontSize"),
				button: {
					textTransform: "none",
				},
			},
			palette: {
				mode: mode,
				primary: {
					main: colors.get("primaryColor"),
				},
				secondary: {
					main: colors.get("secondaryColor"),
				},
				text: {
					primary: colors.get("primaryText"),
					secondary: colors.get("secondaryText"),
				},
				background: {
					main: background,
					paper: background,
					default: background,
				},
			},
		});
		this.forceUpdate();
	}

	render() {
		return (
			<ThemeProvider theme={this.theme}>
				<CssBaseline />
				<DialogProvider>
					<div
						sx={{
							display: "flex",
						}}
					>
						<Router>
							<Routes>
								<Route path="/" exact element={<Login />} />
								<Route path="/ide/" exact element={<IDE />} />
								<Route
									path="/ide?backend=:backend"
									exact
									element={<IDE />}
								/>
								<Route
									path="/ide/classes/:classname"
									exact
									element={<IDE />}
								/>
								<Route
									path="/ide/debuggers/:debugger"
									exact
									element={<IDE />}
								/>
							</Routes>
						</Router>
					</div>
				</DialogProvider>
			</ThemeProvider>
		);
	}
}

export default App;

export { app };
