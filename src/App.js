import React, { Component } from "react";
import { withStyles } from "@material-ui/core/styles";
import {
  Container,
  createMuiTheme,
  CssBaseline
} from "@material-ui/core";
import { ThemeProvider } from "@material-ui/styles";
import { amber } from "@material-ui/core/colors";

import Titlebar from './components/Titlebar'
import Sidebar from './components/Sidebar';
import TabControl from './components/TabControl';
import ClassBrowser from './components/ClassBrowser';


const port = 9000 //window.location.port;
const baseUri = `http://${window.location.hostname}:${port}/bee`;

const drawerWidth = 240;
const useStyles = theme => ({
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
    paddingTop: theme.spacing(1),
    paddingBottom: theme.spacing(1)
  },
  paper: {
//    padding: theme.spacing(1),
//    display: "flex",
    overflow: "auto",
//    flexDirection: "row"
  },
  fixedHeight: {
    height: 240
  }
});

const theme = createMuiTheme({
  typography: {
    fontFamily: '"Segoe UI"',
    fontSize: 14,
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
  }
});

class App extends Component {
  constructor(props){
    super(props);
    this.state = {
      sidebarExpanded: false,
      pages: [
        {
          label: 'Number',
          component: <ClassBrowser baseUri={baseUri} classes={this.props.classes} root='Number'/>
        },
        {
          label: 'ParseNode',
          component: <ClassBrowser baseUri={baseUri} classes={this.props.classes} root='ParseNode'/>
        }
      ]
    }
  }

  expandSidebar = () => {
    this.setState({sidebarExpanded: true});
  };
  
  collapseSidebar = () => {
    this.setState({sidebarExpanded: false});
  };
  
  render () {
    return (
      <ThemeProvider theme={theme}>
        <div className={this.props.classes.root}>
          <CssBaseline/>
          <Titlebar classes={this.props.classes} sidebarExpanded={this.state.sidebarExpanded} expandSidebar={this.expandSidebar} />
          <Sidebar classes={this.props.classes} expanded={this.state.sidebarExpanded} onClose={this.collapseSidebar}/>
          <main className={this.props.classes.content}>
            <div className={this.props.classes.appBarSpacer} />
            <Container maxWidth="lg" className={this.props.classes.container}>
              <TabControl pages={this.state.pages}/>
            </Container>
          </main>
        </div>
      </ThemeProvider>
    )
  }
}

export default withStyles(useStyles)(App);
