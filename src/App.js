import React, { Component } from 'react';
import { withStyles } from '@material-ui/core/styles';
import {
  Container,
  createMuiTheme,
  CssBaseline,
  Grid
} from '@material-ui/core';
import { ThemeProvider } from '@material-ui/styles';
import { amber } from '@material-ui/core/colors';

import Titlebar from './components/Titlebar'
import Sidebar from './components/Sidebar';
import TabControl from './components/TabControl';
import Transcript from './components/Transcript';
import ClassBrowser from './components/ClassBrowser';

const port = 9000 //window.location.port;
const baseUri = `http://${window.location.hostname}:${port}/bee`;

const drawerWidth = 240;
const styles = theme => ({
  root: {
    display: "flex"
  },
  toolbar: {
    paddingRight: 24 // keep right padding when drawer closed
  },
  toolbarIcon: {
    display: "flex",
    alignItems: "center",
    justifyContent: "flex-end",
    padding: "0 8px",
    ...theme.mixins.toolbar
  },
  appBar: {
    color: "primary",
    zIndex: theme.zIndex.drawer + 1,
    transition: theme.transitions.create(["width", "margin"], {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.leavingScreen
    })
  },
  appBarShift: {
    color: "primary",
    marginLeft: drawerWidth,
    width: `calc(100% - ${drawerWidth}px)`,
    transition: theme.transitions.create(["width", "margin"], {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.enteringScreen
    })
  },
  menuButton: {
    marginRight: 36
  },
  menuButtonHidden: {
    display: "none"
  },
  title: {
    flexGrow: 1
  },
  drawerPaper: {
    position: "relative",
    whiteSpace: "nowrap",
    width: drawerWidth,
    transition: theme.transitions.create("width", {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.enteringScreen
    })
  },
  drawerPaperClose: {
    overflowX: "hidden",
    transition: theme.transitions.create("width", {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.leavingScreen
    }),
    width: theme.spacing(7),
    [theme.breakpoints.up("sm")]: {
      width: theme.spacing(9)
    }
  },
  appBarSpacer: theme.mixins.toolbar,
  content: {
    flexGrow: 1,
    height: "100vh",
    overflow: "auto"
  },
  container: {
    paddingTop: theme.spacing(1)
  },
  paper: {
//    padding: theme.spacing(1),
//    display: "flex",
    overflow: "auto",
//    flexDirection: "row"
  },
  fixedHeight: {
    height: 200
  }
});

const theme = createMuiTheme({
  typography: {
    fontFamily: '"Segoe UI"',
    fontSize: 14,
    button: {
      textTransform: 'none'
    }
  },
  palette: {
    type: "dark"
  },
  primary: {
    main: amber[300]
  },
  secondary: {
    main: amber[900]
  },
  codeMirror: {
    fontFamily: '"Arial"',
    fontSize: 24,
  },
  radioLabel: {
    fontSize: 12
  }
});

class App extends Component {
  constructor(props){
    super(props);
    this.state = {
      sidebarExpanded: false,
      pages: ['Collection', 'Magnitude', 'ParseNode', 'WebAPI'].map((c) => {
        return (
          {
            label: c,
            component: <ClassBrowser baseUri={baseUri} classes={this.props.classes} root={c}/>
          })})
    }
  }

  expandSidebar = () => {
    this.setState({sidebarExpanded: true});
  };
  
  collapseSidebar = () => {
    this.setState({sidebarExpanded: false});
  };

  closePage = (page) => {
    this.setState({pages: this.state.pages.filter((p) => {return p.label !== page.label})})
  }
  
  render () {
    return (
      <ThemeProvider theme={theme}>
        <div className={this.props.classes.root}>
          <CssBaseline/>
          <Titlebar classes={this.props.classes} sidebarExpanded={this.state.sidebarExpanded} expandSidebar={this.expandSidebar} />
          <Sidebar classes={this.props.classes} expanded={this.state.sidebarExpanded} onClose={this.collapseSidebar}/>
          <main className={this.props.classes.content}>
            <div className={this.props.classes.appBarSpacer} />
            <Container className={this.props.classes.container}>
              <Grid container spacing={0}>
                <Grid item xs={12} md={9} lg={9}>
                  <Grid container spacing={1}>
                    <Grid item xs={12} md={12} lg={12}>
                      <TabControl pages={this.state.pages} onClose={this.closePage}/>
                    </Grid>
                    <Grid item xs={12} md={12} lg={12}>
                      <Transcript />
                    </Grid>
                  </Grid>
                </Grid>
                <Grid item xs={12} md={3} lg={3}>
                  <p>Inspection area</p>
                </Grid>
              </Grid>
            </Container>
          </main>
        </div>
      </ThemeProvider>
    )
  }
}

export default withStyles(styles)(App);
