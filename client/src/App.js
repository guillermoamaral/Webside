import React, { Component } from "react";
import { withStyles } from "@mui/styles";
import { createTheme } from "@mui/material/styles";
import { ThemeProvider } from "@mui/styles";
import StyledApp from "./StyledApp";

const theme = createTheme({
	typography: {
		//fontFamily: '"Segoe UI"',
		fontSize: 13,
		button: {
			textTransform: "none",
		},
	},
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
			paper: "#303030",
		},
	},
});

class App extends Component {
	render() {
		return (
			<ThemeProvider theme={theme}>
				<StyledApp />
			</ThemeProvider>
		);
	}
}

export default App;
