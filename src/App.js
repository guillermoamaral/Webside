import React, { Component } from "react";
import { withStyles } from "@material-ui/core/styles";
import {
  Container,
  createMuiTheme,
  CssBaseline,
  AppBar,
  Toolbar,
  Avatar,
  Typography,
  IconButton,
} from "@material-ui/core";
import MenuIcon from "@material-ui/icons/Menu";
import { ThemeProvider } from "@material-ui/styles";
import clsx from "clsx";
import { orange } from "@material-ui/core/colors";
import Sidebar from "./components/Sidebar";
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
    zIndex: theme.zIndex.drawer + 1,
    transition: theme.transitions.create(["width", "margin"], {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.leavingScreen
    })
  },
  appBarShift: {
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
    padding: theme.spacing(1),
    display: "flex",
    overflow: "auto",
    flexDirection: "row"
  },
  fixedHeight: {
    height: 240
  }
});

const theme = createMuiTheme({
  typography: {
    fontFamily: '"Segoe UI"'
  },
  palette: {
    type: "dark"
  },
  primary: {
    main: orange[300]
  },
  secondary: {
    main: orange[900]
  }
});

class App extends Component {
  constructor(props){
    super(props);
    this.state = {
      sidebarOpen: false,
    }
  }

  openSidebar = () => {
    this.setState({sidebarOpen: true});
  };
  
  closeSidebar = () => {
    this.setState({sidebarOpen: false});
  };
  
  render () {
    return (
      <ThemeProvider theme={theme}>
        <div className={this.props.classes.root}>
          <CssBaseline/>
          <AppBar
              position="absolute"
              className={clsx(this.props.classes.appBar, this.state.sidebarOpen && this.props.classes.appBarShift)}
            >
            <Toolbar className={this.props.classes.toolbar}>
              <IconButton
                edge="start"
                color="inherit"
                aria-label="open drawer"
                onClick={this.openSidebar}
                className={clsx(
                  this.props.classes.menuButton,
                  this.state.open && this.props.classes.menuButtonHidden
                )}
              >
                <MenuIcon />
              </IconButton>
              <Typography
                component="h1"
                variant="h6"
                color="inherit"
                noWrap
                className={this.props.classes.title}
              >
                Bee Smalltalk Web IDE
              </Typography>
              <Avatar
                alt="Uddeshya Singh"
              />
            </Toolbar>
          </AppBar>

          <Sidebar classes={this.props.classes} open={this.state.sidebarOpen} onClose={this.closeSidebar}/>
          
          <main className={this.props.classes.content}>
            <div className={this.props.classes.appBarSpacer} />
            <Container maxWidth="lg" className={this.props.classes.container}>
              <ClassBrowser classes={this.props.classes} root={'ParseNode'} baseUri={baseUri}/>         
            </Container>
          </main>
        </div>
      </ThemeProvider>
    )
  }
}

export default withStyles(useStyles)(App);
