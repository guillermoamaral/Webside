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
        this.theme = this.defaultTheme();
    }

    defaultTheme() {
        return createTheme(this.defaultThemeOptions());
    }

    defaultThemeOptions() {
        return {
            palette: {
                mode: "dark",
                primary: {
                    main: "#00000",
                },
                secondary: {
                    main: "#ffffff",
                },
                text: {
                    primary: "#aaaaaa",
                    secondary: "#00000",
                },
                background: {
                    main: "#1f1f1f",
                    default: "#1f1f1f",
                },
            },
            components: {
                MuiToolbar: {
                    styleOverrides: {
                        root: {
                            minHeight: "40px",
                            height: "40px",
                        },
                        dense: {
                            minHeight: "40px",
                            height: "40px",
                        },
                    },
                },
            },
            mixins: {
                toolbar: {
                    minHeight: "40px",
                    height: "40px",
                },
            },
        };
    }

    updateTheme(settings) {
        const appearance = settings.section("appearance");
        const mode = appearance.get("mode");
        const colors = appearance.section(mode);
        const background = colors.get("background");
        const options = this.defaultThemeOptions();
        options.palette.mode = mode;
        options.palette.primary.main = colors.get("primaryColor");
        options.palette.secondary.main = colors.get("secondaryColor");
        options.palette.text = {
            primary: colors.get("primaryText"),
            secondary: colors.get("secondaryText"),
        };
        options.palette.background = {
            main: background,
            paper: background,
            default: background,
        };
        options.typography = {
            fontFamily: appearance.get("fontFamily"),
            fontSize: appearance.get("fontSize"),
            button: {
                textTransform: "none",
            },
        };

        this.theme = createTheme(options);
        this.forceUpdate();
    }

    resetTheme() {
        this.theme = this.defaultTheme();
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
                                <Route path="/" element={<Login />} />
                                <Route path="/ide/*" element={<IDE />} />
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
