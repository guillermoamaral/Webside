import React, { Component } from 'react';
import {
  Container,
  createMuiTheme,
  Grid,
  IconButton,
  Menu,
  MenuItem,
  Drawer,
  Box
} from '@material-ui/core';
import { ThemeProvider } from '@material-ui/styles';
import { withCookies } from 'react-cookie';
import { amber, blue } from '@material-ui/core/colors';
import AddIcon from '@material-ui/icons/AddCircle';
import KeyboardArrowDown from '@material-ui/icons/KeyboardArrowDown';
import API from './API';
import { IDEContext } from './IDEContext';
import { DialogProvider } from './dialogs';
import TranscriptIcon from './icons/TranscriptIcon';
import SystemBrowserIcon from './icons/SystemBrowserIcon';
import ClassBrowserIcon from './icons/ClassBrowserIcon';
import MethodBrowserIcon from './icons/MethodBrowserIcon';
import WorkspaceIcon from './icons/WorkspaceIcon';
import InspectorIcon from './icons/InspectorIcon';
import ChangesBrowserIcon from './icons/ChangesBrowserIcon';
import DebuggerIcon from './icons/DebuggerIcon';
import TestRunnerIcon from './icons/TestRunnerIcon';
import ChatIcon from './icons/ChatIcon';
import SettingsIcon from '@material-ui/icons/Settings';
import Titlebar from './layout/Titlebar'
import Sidebar from './layout/Sidebar';
import TabControl from './controls/TabControl';
import Transcript from './tools/Transcript';
import SystemBrowser from './tools/SystemBrowser';
import ClassBrowser from './tools/ClassBrowser';
import MethodBrowser from './tools/MethodBrowser';
import Inspector from './tools/Inspector';
import Workspace from './tools/Workspace';
import ChangesBrowser from './tools/ChangesBrowser';
import Debugger from './tools/Debugger';
import TestRunner from './tools/TestRunner';
import Profiler from './tools/Profiler';
import NativeDebugger from './tools/NativeDebugger';
import ChatClient from './ChatClient';
import Chat from './tools/Chat';
import Settings from './Settings'

class IDE extends Component {
  constructor(props){
    super(props);
    this.updateSettings();
    this.updateTheme(); 
    this.initializeAPI();
    this.initializeChat();
    this.state = {
      sidebarExpanded: false,
      addPageMenuOpen: false,
      selectedPage: null,
      transcriptOpen: false,
      unreadErrorsCount: 0,
      transcriptText: this.welcomeMessage(),
      pages: [],
      projectNames: [],
      classNames: [],
    }
  }

  componentDidMount() {
    this.getNames();
    //this.openNativeDebugger('{B3AE5087-3EBC-43E2-B4A5-95DD37D802FE}')
  }

  updateSettings() {
    const cookies = this.props.cookies;
    this.dialect = cookies.get('dialect');
    this.baseUri = cookies.get('baseUri');
    this.developer = cookies.get('developer');
    this.chatUrl = 'http://localhost:4200';
  }

  welcomeMessage() {
    const backend = this.dialect !== 'undefined'? this.dialect : 'It looks like the Smalltalk system could not be determined';
    return '\'Welcome to Webside ' 
      + this.developer
      + '!\'\r\'A Smalltalk IDE for the web.\'\r\r'
      + '\'Backend: ' + backend + '\'\r'
      + '\'@' + this.baseUri + '\'';
  }

  initializeAPI() {
    this.api = new API(this.baseUri, this.developer, this.reportError, this.reportChange);
  }

  initializeChat(){
    this.chatClient = new ChatClient();
    this.chatClient.login(this.chatUrl, this.developer);
  }

  updateTheme() {
    var mainPrimaryColor;
    var mainSecondaryColor;
    switch (this.dialect) {
      case "Bee": 
        mainPrimaryColor = amber[300];
        mainSecondaryColor = amber[800];
        break;  
      case "Pharo":
        mainPrimaryColor = blue[300];
        mainSecondaryColor = blue[800];
        break;
      default:
        mainPrimaryColor = "#00000";
        mainSecondaryColor = "#00000";
    }
    this.theme = createMuiTheme({
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
  }

  getNames = async () => {
    var projectNames, classNames;
    try {
      projectNames = await this.api.getProjectNames();
    }
    catch (error) {
      projectNames = [];
      this.reportError(error)
    }
    try {
      classNames = await this.api.getClassNames();
    }
    catch (error) {
      classNames = [];
      this.reportError(error)
    }
    this.setState({projectNames: projectNames, classNames: classNames})
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

  selectPage = (page) => {
    this.setState({selectedPage: page})
  }

  removePage = async (page) => {
    try {
      if (page.component.type.name === 'Inspector') {
        await this.api.unpinObject(page.component.props.id)
      }
      if (page.component.type.name === 'Debugger') {
        await this.api.deleteDebugger(page.component.props.id)
      }
      if (page.component.type.name === 'TestRunner') {
        await this.api.deleteTestRun(page.component.props.id)
      }
      if (page.component.type.name === 'Workspace') {
        await this.api.deleteWorkspace(page.component.props.id)
      }
    } catch(error) {this.reportError(error)}
    const {pages, selectedPage} = this.state;
    let i = pages.indexOf(page);
    const j = pages.indexOf(selectedPage);
    const selected = (i <= j)? pages[Math.max(i - 1, 0)] : selectedPage;
    this.setState({pages: pages.filter(p => p !== page), selectedPage: selected})
  }

  openTranscript() {
    const transcript = <Transcript styles={this.props.styles} text={this.state.transcriptText}/>;
    this.addPage('Transcript', <TranscriptIcon />, transcript);
  }

  openInspectors() {
    this.api.getObjects()
      .then(objects => {objects.forEach(o => this.openInspector(o))})
      .catch(error => {})
  }

  openSystemBrowser = (projectname) => {
    const browser = <SystemBrowser styles={this.props.styles} root={projectname}/>;
    this.addPage(browser.props.root || 'System Browser', <SystemBrowserIcon/>, browser);
  }

  openClassBrowser = (classname) => {
    const browser = <ClassBrowser styles={this.props.styles} root={classname}/>;
    this.addPage(browser.props.root || 'Class Browser', <ClassBrowserIcon/>, browser);
  }

  openMethodBrowser = (methods, title = 'Methods') => {
    const browser = <MethodBrowser styles={this.props.styles} methods={methods}/>;
    this.addPage(title + ' (' + methods.length + ')', <MethodBrowserIcon/>, browser);
  }

  openWorkspace = (id) => {
    const workspace = <Workspace styles={this.props.styles} key={id} id={id}/>;
    this.addPage('Workspace', <WorkspaceIcon/>, workspace);
  }

  openDebugger = (id, title = 'Debugger') => {
    const tool = <Debugger styles={this.props.styles} key={id} id={id}/>;
    this.addPage(title, <DebuggerIcon/>, tool);
  }

  closeDebugger = (id) => {
    const page = this.state.pages.find(p => p.component.type.name === 'Debugger' && p.component.props.id === id);
    if (page) {this.removePage(page)}
  }

  openInspector = (object) => {
    const inspector = <Inspector styles={this.props.styles} key={object.id} root={object} id={object.id} showWorkspace/>;
    this.addPage('Inspecting: ' + object.class, <InspectorIcon/>, inspector);
  }

  openChangesBrowser = (changes, title = 'Changes') => {
    const browser = <ChangesBrowser styles={this.props.styles} changes={changes}/>;
    this.addPage(title + ' (' + changes.length + ')', <ChangesBrowserIcon className={this.props.styles.changesBrowserIcon} />, browser);
  }

  openTestRunner = (id, title = 'Test Runner') => {
    const tool = <TestRunner styles={this.props.styles} key={id} id={id}/>;
    this.addPage(title, <TestRunnerIcon className={this.props.styles.testRunnerIcon} />, tool);
  }

  openProfiler = (id, title = 'Profiler') => {
    const tool = <Profiler styles={this.props.styles} key={id} id={id}/>;
    this.addPage(title, <TestRunnerIcon className={this.props.styles.testRunnerIcon} />, tool);
  }

  openNativeDebugger = (id, title = 'Native Debugger') => {
    const tool = <NativeDebugger styles={this.props.styles} key={id} id={id}/>;
    this.addPage(title, <DebuggerIcon/>, tool);
  }

  openChat = (contactname) => {
    if (contactname === this.developer) return;
    const contact = this.chatClient.contactNamed(contactname);
    if (contactname && !contact) return;
    const page = this.state.pages.find(p => p.label === 'Chat');
    if (page) {
      this.selectPage(page);
    } else {
      const tool = <Chat styles={this.props.styles} client={this.chatClient} initialContact={contact}/>;
      this.addPage('Chat', <ChatIcon/>, tool);
    }
  }

  openSettings = () => {
    var page = this.state.pages.find(p => p.label === 'Configuration');
    if (page) {
      this.selectPage(page);
    } else {
      const settings = <Settings
        styles={this.props.styles}
        baseUri={this.baseUri}
        developer={this.developer}
        onSave={() => {
          this.updateSettings();
          this.updateTheme(); 
          this.initializeAPI();
          this.initializeChat();
          page = this.state.pages.find(p => p.label === 'Configuration');
          this.removePage(page);
        }}/>;
      this.addPage('Configuration', <SettingsIcon/>, settings);
    }
  }
  
  browseSenders = async (selector) => {
    try {
      const senders = await this.api.getSenders(selector);
      this.openMethodBrowser(senders, 'Senders of ' + selector);
    } catch(error) {this.reportError(error)} 
  }

  browseLocalSenders = async (selector, classname) => {
    try {
      const senders = await this.api.getLocalSenders(selector, classname);
      this.openMethodBrowser(senders, 'Local senders of ' + selector);
    } catch(error) {this.reportError(error)}
  }

  browseImplementors = async (selector) => {
    try {
      const implementors = await this.api.getImplementors(selector);
      this.openMethodBrowser(implementors, 'Implementors of ' + selector);
    } catch(error) {this.reportError(error)}
  }

  browseLocalImplementors = async (selector, classname) => {
    try {
      const implementors = await this.api.getLocalImplementors(selector, classname);
      this.openMethodBrowser(implementors, 'Local implementors of ' + selector);
    } catch(error) {this.reportError(error)} 
  }

  browseReferences = async (classname) => {
    try {
      const references = await this.api.getReferences(classname);
      this.openMethodBrowser(references, 'References to ' + classname);
    } catch(error) {this.reportError(error)} 
  }

  browseLastChanges = async () => {
    try {
      const changes = await this.api.getChanges();
      this.openChangesBrowser(changes, 'Last changes');
    }
    catch (error) {this.reportError(error)}
  }

  debugExpression = async (expression, context) => {
    try {
      const id = await this.api.debugExpression(expression, context);
      this.openDebugger(id, 'Debugging expression');
    } catch(error) {this.reportError(error)}
  }

  evaluateExpression = async (expression, sync, pin, context) => {
    try {
      const result = await this.api.evaluateExpression(expression, sync, pin, context);
      if (sync) {return result}
      const object = await this.api.getObject(result.id);
      if (!pin && !sync) {await this.api.unpinObject(object.id)}
      return object;
    }
    catch (error) {
      if (error.data && error.data.process) {
        // const debug = await this.confirm(error.description, 'Stack tracke:\r' + error.stack + '\r\rDo you want to debug it?');
        // (debug)? this.openDebugger(error.debugger) : this.reportError(error.description);
        const id = await this.api.createDebugger(error.data.process)
        this.openDebugger(id);
      }
    }
  }

  runTest = async (classname, selector) => {
    try {
      const status = await this.api.runTest(classname, selector);
      this.openTestRunner(status.id, 'Test ' + selector);
    } catch(error) {this.reportError(error)}
  }

  runTestClass = async (classname) => {
    try {
      const status = await this.api.runTestClass(classname);
      this.openTestRunner(status.id, 'Test ' + classname);
    } catch(error) {this.reportError(error)}
  }

  runTestProject = async (projectname) => {
    try {
      const status = await this.api.runTestProject(projectname);
      this.openTestRunner(status.id, 'Test ' + projectname);
    } catch(error) {this.reportError(error)}
  }

  profileExpression = async (expression, context) => {
    try {
      const id = await this.api.profileExpression(expression, context);
      this.openProfiler(id);
    } catch(error) {this.reportError(error)}
  }

  expandSidebar = () => {
    this.setState({sidebarExpanded: true});
  }
  
  collapseSidebar = () => {
    this.setState({sidebarExpanded: false});
  }

  reportError = (text) => {
    if (!text) {return}
    this.setState(
      {
        transcriptText: this.state.transcriptText + '\r' + text,
        unreadErrorsCount: this.state.unreadErrorsCount + 1,
    })
  }

  reportChange = async (change) => {
    //this triggers unnecessary renders!!!
    // const changes = await this.api.getChanges(); 
    // this.setState({changesCount: changes.length})
  }

  addSystemBrowserClicked = () => {
    this.setState({addPageMenuOpen: false});
    this.openSystemBrowser();
  }

  addClassBrowserClicked = () => {
    this.setState({addPageMenuOpen: false});
    this.openClassBrowser();
  }

  addWorkspaceClicked = async () => {
    this.setState({addPageMenuOpen: false})
    try {
      const id = await this.api.createWorkspace();
      this.openWorkspace(id)
    }
    catch (error) {this.reportError(error)}
  }

  toggleShowTranscript = () => {
    this.setState({transcriptOpen: !this.state.transcriptOpen, unreadErrorsCount: 0});
  }

  render() {
    console.log('rendering IDE')
    const context = {
      api: this.api,
      projectNames: this.state.projectNames,
      classNames: this.state.classNames,
      openChat: this.openChat,
      browseProject: this.openSystemBrowser,
      browseClass: this.openClassBrowser,
      browseSenders: this.browseSenders,
      browseLocalSenders: this.browseLocalSenders,
      browseImplementors: this.browseImplementors,
      browseLocalImplementors: this.browseLocalImplementors,
      browseReferences: this.browseReferences,
      evaluateExpression: this.evaluateExpression,
      debugExpression: this.debugExpression,
      profileExpression: this.profileExpression,
      runTest: this.runTest,
      runTestClass: this.runTestClass,
      runTestProject: this.runTestProject,
      openDebugger: this.openDebugger,
      closeDebugger: this.closeDebugger,
      inspectObject: this.openInspector,
      reportError: this.reportError};
    const styles = this.props.styles;
    return (
      <IDEContext.Provider value={context}>
        <ThemeProvider theme={this.theme}>
          <DialogProvider>
            <div className={styles.root}>
              <Titlebar
                developer={this.developer}
                dialect={this.dialect}
                styles={styles}
                sidebarExpanded={this.state.sidebarExpanded}
                expandSidebar={this.expandSidebar}
                searchOptions={this.state.classNames || []}
                onAvatarClicked={this.openSettings}/>
              <Sidebar
                styles={styles}
                expanded={this.state.sidebarExpanded}
                unreadErrorsCount={this.state.unreadErrorsCount}
                onTranscriptClicked={this.toggleShowTranscript}
                onChangesClicked={this.browseLastChanges}
                onPeersClicked={this.openChat}
                onSettingsClicked={this.openSettings}
                onClose={this.collapseSidebar}/>
              <main className={styles.content}>
                <div className={styles.appBarSpacer} />
                <Container className={styles.container}>
                  <Grid container spacing={1}>
                    <Grid item xs={11} md={11} lg={11}>
                        <TabControl
                          styles={styles}
                          selectedPage={this.state.selectedPage}
                          pages={this.state.pages}
                          onSelect={this.selectPage}
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
                          <MenuItem onClick={this.addSystemBrowserClicked}>
                            <Box display="flex" flexWrap="nowrap" alignItems="center" justifyContent="center">
                              <Box pt={1} pr={1}>
                                <SystemBrowserIcon/>
                              </Box>
                              <Box>
                                System Browser
                              </Box>
                            </Box>
                          </MenuItem>
                          <MenuItem onClick={this.addClassBrowserClicked}>
                            <Box display="flex" flexWrap="nowrap" alignItems="center" justifyContent="center">
                              <Box pt={1} pr={1}>
                                <ClassBrowserIcon/>
                              </Box>
                              <Box>
                                Class Browser
                              </Box>
                            </Box>
                          </MenuItem>
                          <MenuItem onClick={this.addWorkspaceClicked}>
                            <Box display="flex" flexWrap="nowrap" alignItems="center" justifyContent="center">
                              <Box pt={1} pr={1}>
                                <WorkspaceIcon/>
                              </Box>
                              <Box>
                                Workspace
                              </Box>
                            </Box>
                          </MenuItem>
                      </Menu>
                    </Grid>
                    <React.Fragment key="bottom">
                      <Drawer
                        anchor="bottom"
                        variant="persistent"
                        open={this.state.transcriptOpen}>
                        <Grid container spacing={0}>
                          <Grid item xs={11} md={11} lg={11}> 
                            <Transcript
                              styles={styles}
                              text={this.state.transcriptText}
                              onChange={text => this.setState({transcriptText: text})}/>
                          </Grid>
                          <Grid item xs={1} md={1} lg={1}>
                            <Box display="flex" justifyContent="center" > 
                              <IconButton onClick={() => this.setState({transcriptOpen: false})}>
                                  <KeyboardArrowDown/>
                              </IconButton>
                            </Box>
                          </Grid>
                        </Grid>
                      </Drawer>
                    </React.Fragment>
                  </Grid>
                </Container>
              </main>
            </div>
          </DialogProvider>
        </ThemeProvider>
      </IDEContext.Provider>
    )
  }
}

export default withCookies(IDE);
