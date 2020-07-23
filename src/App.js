import React, { Component } from 'react';
import { withStyles } from '@material-ui/core/styles';
import {
  Container,
  createMuiTheme,
  CssBaseline,
  Grid,
  IconButton,
  Menu,
  MenuItem,
  Drawer,
  Dialog
} from '@material-ui/core';
import { ThemeProvider } from '@material-ui/styles';
import { amber , blue } from '@material-ui/core/colors';
import AddIcon from '@material-ui/icons/AddCircle';
import API from './components/API';
import { AppContext } from './AppContext';
import TranscriptIcon from './components/icons/TranscriptIcon';
import ClassBrowserIcon from './components/icons/ClassBrowserIcon';
import MethodBrowserIcon from './components/icons/MethodBrowserIcon';
import WorkspaceIcon from './components/icons/WorkspaceIcon';
import InspectorIcon from './components/icons/InspectorIcon';
import ChangesBrowserIcon from './components/icons/ChangesBrowserIcon';
import DebuggerIcon from './components/icons/DebuggerIcon';

import ConfirmDialog from './components/controls/ConfirmDialog';
import Titlebar from './components/layout/Titlebar'
import Sidebar from './components/layout/Sidebar';
import TabControl from './components/controls/TabControl';
import Transcript from './components/tools/Transcript';
import ClassBrowser from './components/tools/ClassBrowser';
import MethodBrowser from './components/tools/MethodBrowser';
import Inspector from './components/tools/Inspector';
import Workspace from './components/tools/Workspace';
import ChangesBrowser from './components/tools/ChangesBrowser';
import Debugger from './components/tools/Debugger';

const smalltalk = 'Bee';
var port;
var baseUri;
var mainPrimaryColor;
var mainSecondaryColor;

switch (smalltalk) {
  case "Bee": 
    port = 9000 //window.location.port;
    baseUri = `http://${window.location.hostname}:${port}/bee`;
    mainPrimaryColor = amber[300];
    mainSecondaryColor = amber[800];
    break;  
  case "Pharo":
    port = 9001 //window.location.port;
    baseUri = `http://${window.location.hostname}:${port}/pharo`;
    mainPrimaryColor = blue[300];
    mainSecondaryColor = blue[800];
    break;
  default:
}

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
    paddingTop: theme.spacing(0),
    paddingLeft: theme.spacing(0),
  },
  paper: {
    padding: theme.spacing(1),
    //display: "flex",
    overflow: "auto",
    width: '100%',
    //maxHeight: 240,
    //flexDirection: "row."
    //color: theme.palette.secondary.main,
    backgroundColor: theme.palette.primary.background,
  },
  box: {
    //backgroundColor: 'white',
  },
  tabControl: {
    flexGrow: 1,
    width: '100%',
    backgroundColor: theme.palette.primary.background,
  },
  tabIcon: {
    display: "flex",
    color: "secondary",
    alignItems: "left",
    justifyContent: "flex-end",
    paddingTop: "8px",
    fontSize: "small", 
  },
  fixedHeight: {
    height: 200
  },
  codeMirror: {
    fontFamily: theme.typography.fontFamily,
    fontSize: theme.typography.fontSize + 2,
    minHeight: 200,
    //position: "inherit",
    top: 0,
    bottom: 0,
    left: 0,
    right: 0,
    height: "auto",
  },
  grow: {
    flexGrow: 1
  },
});

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
      //background: "303030"
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
  }
});

class App extends Component {
  constructor(props){
    super(props);
    this.api = new API(baseUri, 'guest', this.reportError);
    this.state = {
      sidebarExpanded: false,
      addPageMenuOpen: false,
      selectedPage: null,
      transcriptOpen: false,
      transcriptText: 'Wellcome!\r\rThis is Webtalk, a web Smalltalk IDE built with React.',
      pages: []
    }
  }

  componentDidMount() {
    this.openDebugger(61849);
    // this.openInspectors();
    // this.openWorkspace();
    // this.openClassBrowser('Magnitude');
  }

  confirm = (title, question) => {
    return new Promise((resolve, reject) => {
      this.confirmDialog.setState({
        title: title,
        question: question,
        onConfirm: () => resolve(true),
        onCancel: () => resolve(false)
      })
    }) 
  }

  addPage(label, icon, component) {
    const page = {
      label: label,
      icon: icon,
      component: component};
    const pages = this.state.pages;
    pages.push(page);
    this.setState({pages: pages, selectedPage: pages.length - 1})
  }

  removePage = (page) => {
    //console.log(page.component.type === Inspector)
    this.setState({pages: this.state.pages.filter(p => {return p !== page})})
  }

  openTranscript() {
    const transcript = <Transcript classes={this.props.classes} text={this.state.transcriptText}/>;
    this.addPage('Transcript', <TranscriptIcon />, transcript);
  }

  openInspectors() {
    this.api.getObjects()
      .then(objects => {objects.forEach(o => this.openInspector(o))})
      .catch(error => {})
  }

  openClassBrowser = (classname) => {
    const root = (classname === undefined)? 'Magnitude' : classname;
    const browser = <ClassBrowser classes={this.props.classes} root={root}/>;
    this.addPage(root, <ClassBrowserIcon className={this.props.classes.classBrowserIcon} />, browser);
  }

  openMethodBrowser = (methods, title = 'Methods') => {
    const browser = <MethodBrowser classes={this.props.classes} methods={methods}/>;
    this.addPage(title + '(' + methods.length + ')', <MethodBrowserIcon className={this.props.classes.methodBrowserIcon} />, browser);
  }

  openWorkspace = () => {
    const workspace = <Workspace classes={this.props.classes}/>;
    this.addPage('Workspace', <WorkspaceIcon className={this.props.classes.workspaceIcon} />, workspace);
  }

  openDebugger = (id) => {
    const tool = <Debugger
      classes={this.props.classes}
      key={id}
      id={id}/>;
    this.addPage('Debugger: ' + id, <DebuggerIcon className={this.props.classes.debuggerIcon} />, tool);
  }

  openInspector = (object) => {
    const inspector = <Inspector
      classes={this.props.classes}
      key={object.id}
      root={object}/>;
    this.addPage(object.class + ': ' + object.id, <InspectorIcon className={this.props.classes.workspaceIcon} />, inspector);
  }

  openChangesBrowser = () => {
    const browser = <ChangesBrowser classes={this.props.classes}/>;
    this.addPage('Last Changes', <ChangesBrowserIcon className={this.props.classes.changesBrowserIcon} />, browser);
  }

  browseSenders = (selector) => {
    this.api.getSenders(selector)
      .then(methods => this.openMethodBrowser(methods, 'Senders of ' + selector)); 
  }

  browseImplementors = (selector) => {
    this.api.getImplementors(selector)
      .then(methods => this.openMethodBrowser(methods, 'Implementors of ' + selector)); 
  }

  browseReferences = (classname) => {
    this.api.getReferences(classname)
      .then(methods => this.openMethodBrowser(methods, 'References to ' + classname)); 
  }

  expandSidebar = () => {
    this.setState({sidebarExpanded: true});
  };
  
  collapseSidebar = () => {
    this.setState({sidebarExpanded: false});
  };

  reportError = (text) => {
    this.setState(
      {
        transcriptText: this.state.transcriptText + '\r' + text,
        transcriptOpen: true,
    })
  }

  addClassBrowserClicked = () => {
    this.setState({addPageMenuOpen: false})
    this.openClassBrowser('Magnitude')
  }

  addWorkspaceClicked = () => {
    this.setState({addPageMenuOpen: false})
    this.openWorkspace()
  }

  toggleShowTranscript = () => {
    this.setState({transcriptOpen: !this.state.transcriptOpen});
  }

  render() {
    const context = {
      api: this.api,
      browseSenders: this.browseSenders,
      browseImplementors: this.browseImplementors,
      browseClass: this.openClassBrowser,
      browseReferences: this.browseReferences,
      inspectObject: this.openInspector,
      reportError: this.reportError};

    return (
      <ThemeProvider theme={theme}>
        <AppContext.Provider value={context}>
          <div className={this.props.classes.root}>
            <CssBaseline/>
            <Titlebar
              title={smalltalk + ' Web IDE'}
              appName={smalltalk}
              classes={this.props.classes}
              sidebarExpanded={this.state.sidebarExpanded}
              expandSidebar={this.expandSidebar}/>
            <Sidebar
              classes={this.props.classes}
              expanded={this.state.sidebarExpanded}
              onTranscript={this.toggleShowTranscript}
              onChanges={this.openChangesBrowser}
              onClose={this.collapseSidebar}/>
            <main className={this.props.classes.content}>
              <div className={this.props.classes.appBarSpacer} />
              <Container className={this.props.classes.container}>
                <Grid container spacing={1}>
                  <Grid item xs={11} md={11} lg={11}>
                      <TabControl
                        classes={this.props.classes}
                        selectedPage={this.state.selectedPage}
                        pages={this.state.pages}
                        onClose={this.removePage}/>
                  </Grid>
                  <Grid item xs={1} md={1} lg={1}>
                    <IconButton id="addPageButton" color="primary" onClick={() => {this.setState({addPageMenuOpen: true})}}>
                      <AddIcon style={{fontSize: 40}}/>
                    </IconButton>
                    <Menu
                      id="addPageMenu"
                      anchorEl={document.getElementById("addPageButton")}
                      keepMounted
                      open={this.state.addPageMenuOpen}
                      onClose={() => {this.setState({addPageMenuOpen: false})}}>
                      <MenuItem onClick={this.addClassBrowserClicked}>
                        <ClassBrowserIcon />
                        Class Browse
                      </MenuItem>
                      <MenuItem onClick={this.addWorkspaceClicked}>
                        <WorkspaceIcon />
                        New Workspace
                      </MenuItem>
                    </Menu>
                  </Grid>
                  <React.Fragment key="bottom">
                    <Drawer
                      anchor="bottom"
                      variant="persistent"
                      open={this.state.transcriptOpen}
                      onClose={() => this.setState({transcriptOpen: false})}>
                      <Transcript
                        classes={this.props.classes}      
                        text={this.state.transcriptText}/>
                    </Drawer>
                  </React.Fragment>
                </Grid>
              </Container>
            </main>
          </div>
        </AppContext.Provider>
      </ThemeProvider>
    )
  }
}

export default withStyles(styles)(App);
