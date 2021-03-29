import React, { Component } from 'react';
import {
  BrowserRouter as Router,
  Switch,
  Route} from "react-router-dom";
import { withStyles } from '@material-ui/core/styles';
import { createMuiTheme, CssBaseline } from '@material-ui/core';
import styles from './styles';
import { ThemeProvider } from '@material-ui/styles';
import { DialogProvider } from './components/dialogs/index';
import { CookiesProvider } from 'react-cookie';
import Login from './components/Login';
import IDE from './components/IDE';

const theme = createMuiTheme({
  typography: {
    //fontFamily: '"Segoe UI"',
    fontSize: 13,
    button: {
      textTransform: "none"
    }
  },
  palette: {
    type: "dark",
    primary: {
      main: "#00000",
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
                      <Route path="/" exact component={() => <Login styles={this.props.classes}/>}/>
                      <Route path="/ide/" exact component={() => <IDE styles={this.props.classes}/>}/>
                      <Route path="/ide/classes/:classname" exact component={() => <IDE styles={this.props.classes}/>}/>
                      <Route path="/ide/debuggers/:debuggerid" exact component={() => <IDE styles={this.props.classes}/>}/>
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
