import React, { Component } from "react";
import { createMuiTheme, CssBaseline } from "@material-ui/core";
import { ThemeProvider } from "@material-ui/styles";
import Webside from "./Webside";

const theme = createMuiTheme({
	typography: {
		//fontFamily: '"Segoe UI"',
		fontSize: 13,
		button: {
			textTransform: "none",
		},
	},
	palette: {
		type: "dark",
		primary: {
			main: "#00000",
		},
		text: {
			primary: "#aaaaaa",
			secondary: "#00000",
		},
		background: {
			paper: "#303030",
		},
	},
});

class App extends Component {
	render() {
		return (
			<ThemeProvider theme={theme}>
				<CssBaseline />
				<Webside />
			</ThemeProvider>
		);
	}
}

export default App;
