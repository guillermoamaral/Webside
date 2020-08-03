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
  Drawer
} from '@material-ui/core';
import styles from './styles';
import { ThemeProvider } from '@material-ui/styles';
import { amber, blue } from '@material-ui/core/colors';
import AddIcon from '@material-ui/icons/AddCircle';
import API from './components/API';
import { AppContext } from './AppContext';
import { DialogProvider } from './components/dialogs';
import TranscriptIcon from './components/icons/TranscriptIcon';
import ClassBrowserIcon from './components/icons/ClassBrowserIcon';
import MethodBrowserIcon from './components/icons/MethodBrowserIcon';
import WorkspaceIcon from './components/icons/WorkspaceIcon';
import InspectorIcon from './components/icons/InspectorIcon';
import ChangesBrowserIcon from './components/icons/ChangesBrowserIcon';
import DebuggerIcon from './components/icons/DebuggerIcon';
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
  },
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
      transcriptText: 'Wellcome to Webside!\r\rA web Smalltalk IDE built with ReactJS.',
      pages: []
    }
  }

  componentDidMount() {
    this.getClassNames();
    // this.openWorkspace();
    // this.openDebugger(61849);
    // this.openInspectors();
    // this.openClassBrowser('Magnitude');
  }

  getClassNames = async () => {
    const names = await this.api.getClassNames();
    this.setState({classNames: names})
  }

  addPage(label, icon, component) {
    const page = {
      label: label,
      icon: icon,
      component: component};
    const pages = this.state.pages;
    pages.push(page);
    this.setState({pages: pages, selectedPage: page})
  }

  pageSelected = (page) => {
    this.setState({selectedPage: page})
  }

  removePage = (page) => {
    //console.log(page.component.type === Inspector)
    const {pages, selectedPage} = this.state;
    let i = pages.indexOf(page);
    const j = pages.indexOf(selectedPage);
    const selected = (i <= j)? pages[Math.max(i - 1, 0)] : selectedPage;
    this.setState({pages: pages.filter(p => p !== page), selectedPage: selected})
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
    const root = classname || 'Magnitude';
    const browser = <ClassBrowser classes={this.props.classes} root={root}/>;
    this.addPage(root, <ClassBrowserIcon className={this.props.classes.classBrowserIcon} />, browser);
  }

  openMethodBrowser = (methods, title = 'Methods') => {
    const browser = <MethodBrowser classes={this.props.classes} methods={methods}/>;
    this.addPage(title + ' (' + methods.length + ')', <MethodBrowserIcon className={this.props.classes.methodBrowserIcon} />, browser);
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

  closeDebugger = (id) => {
    console.log(id)
    console.log(this.state.pages.component.type[1])
    const page = this.state.pages.find(p => p.component.type === Debugger && p.component.id === id);
    if (page) {this.removePage(page)}
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

  browseLocalSenders = (selector, classname) => {
    this.api.getLocalSenders(selector, classname)
      .then(methods => this.openMethodBrowser(methods, 'Local senders of ' + selector)); 
  }

  browseImplementors = (selector) => {
    this.api.getImplementors(selector)
      .then(methods => this.openMethodBrowser(methods, 'Implementors of ' + selector)); 
  }

  browseLocalImplementors = (selector, classname) => {
    this.api.getLocalImplementors(selector, classname)
      .then(methods => this.openMethodBrowser(methods, 'Local implementors of ' + selector)); 
  }

  browseReferences = (classname) => {
    this.api.getReferences(classname)
      .then(methods => this.openMethodBrowser(methods, 'References to ' + classname)); 
  }

  debugExpression = async (expression) => {
    const id = await this.api.debug(expression);
    this.openDebugger(id)
  }

  evaluateExpression = async (expression, pin) => {
    try {
      const object = await this.api.evaluate(expression, pin);
      return object;
    }
    catch (error) {
      this.openDebugger(error.debugger)
      // const debug = await this.confirm(error.description, 'Stack tracke:\r' + error.stack + '\r\rDo you want to debug it?');
      // (debug)? this.openDebugger(error.debugger) : this.reportError(error.description);
    }
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
      classNames: this.state.classNames,
      browseClass: this.openClassBrowser,
      browseSenders: this.browseSenders,
      browseLocalSenders: this.browseLocalSenders,
      browseImplementors: this.browseImplementors,
      browseLocalImplementors: this.browseLocalImplementors,
      browseReferences: this.browseReferences,
      evaluateExpression: this.evaluateExpression,
      debugExpression: this.debugExpression,
      closeDebugger: this.closeDebugger,
      inspectObject: this.openInspector,
      reportError: this.reportError};

    return (
      <ThemeProvider theme={theme}>
        <AppContext.Provider value={context}>
          <DialogProvider>
            <div className={this.props.classes.root}>           
              <CssBaseline/>
              <Titlebar
                title={smalltalk + ' Web IDE (Powered by BESIDE)'}
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
                          onSelect={this.pageSelected}
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
                          text={this.state.transcriptText}
                          onChange={text => this.setState({transcriptText: text})}/>
                      </Drawer>
                    </React.Fragment>
                  </Grid>
                </Container>
              </main>
            </div>
          </DialogProvider>
        </AppContext.Provider>
      </ThemeProvider>
    )
  }
}

export default withStyles(styles)(App);
