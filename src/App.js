import React, { Component } from 'react';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  } from "react-router-dom";
import { withStyles } from '@material-ui/core/styles';
import { createMuiTheme, CssBaseline } from '@material-ui/core';
import styles from './styles';
import { ThemeProvider } from '@material-ui/styles';
import { DialogProvider } from './components/dialogs';
import { CookiesProvider } from 'react-cookie';
import { amber, blue } from '@material-ui/core/colors';
import Connect from './Connect';
import IDE from './IDE';

const smalltalk = 'Bee';
var mainPrimaryColor;
var mainSecondaryColor;

switch (smalltalk) {
  case "Bee": 
    mainPrimaryColor = amber[300];
    mainSecondaryColor = amber[800];
    break;  
  case "Pharo":
    mainPrimaryColor = blue[300];
    mainSecondaryColor = blue[800];
    break;
  default:
}

const theme = createMuiTheme({
  typography: {
    fontFamily: '"Segoe UI"',
    fontSize: 13,
    button: {
      textTransform: "none"
    }
  },
  palette: {
    type: "dark",
    primary: {
      main: mainPrimaryColor,
    },
    secondary: {
      main: mainSecondaryColor,
    },
    text: {
      primary: "#aaaaaa",
      secondary: "#00000"
    },
    background: {
      paper: '#303030',
    }
  },
})

class App extends Component {  
  render() { 
    return (
      <ThemeProvider theme={theme}>
        <CssBaseline/>
        <DialogProvider>
          <CookiesProvider>
            <div className={styles.root}>           
              <Router>
                  <Switch>
                      <Route path="/" exact component={() => <Connect styles={this.props.classes}/>}/>
                      <Route path="/ide/" exact component={() => <IDE styles={this.props.classes}/>}/>
                    </Switch>
                </Router>
              </div>
            </CookiesProvider>  
        </DialogProvider>
      </ThemeProvider>
    )
  }
}

export default withStyles(styles)(App);
