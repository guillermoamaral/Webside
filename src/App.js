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

import API from './components/API';

import TranscriptIcon from '@material-ui/icons/CallToAction';
import ClassBrowserIcon from '@material-ui/icons/AccountTree';
import MethodBrowserIcon from '@material-ui/icons/Reorder';
import WorkspaceIcon from '@material-ui/icons/Code';

import Titlebar from './components/Titlebar'
import Sidebar from './components/Sidebar';
import TabControl from './components/TabControl';
import Transcript from './components/Transcript';
import ClassBrowser from './components/ClassBrowser';
import MethodBrowser from './components/MethodBrowser';
import Inspector from './components/Inspector';
import Workspace from './components/Workspace';

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
    paddingTop: theme.spacing(0),
    paddingLeft: theme.spacing(0)
  },
  paper: {
    padding: theme.spacing(1),
    //display: "flex",
    overflow: "auto",
    //maxHeight: 240,
    //flexDirection: "row"
  },
  tabControl: {
    flexGrow: 1,
    width: '100%',
    //backgroundColor: theme.palette.background.paper,
  },
  tabIcon: {
    //display: "flex",
    color: "primary",
    alignItems: "left",
    justifyContent: "flex-end",
    paddingTop: "4px",
    fontSize: "small", 
  },
  radioGroup: {
    fontSize: 10,
    width: 'auto',
    height: 'auto',
    display: 'flex',
    //flexWrap: 'nowrap',
    //flexDirection: 'row'
  },
  radioButton: {
    fontSize: 10,
  },
  radioLabel: {
    fontSize: 10
  },
  fixedHeight: {
    height: 200
  },
  fixedHeight2: {
    height: 154
  },
  transcriptIcon: {
    color: '#eebd00',
  },
  classBrowserIcon: {
    color: '#f3504b',
  },
  methodBrowserIcon: {
    color: '#2bb9dd',
  },
  workspaceIcon: {
    color: '#f28285',
  },
  codeMirror: {
    fontFamily: theme.typography.fontFamily,
    fontSize: "20px",
    backgroundColor: theme.palette.background.paper,
    //height: "100%",
    //maxHeight: 200,
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
      textTransform: 'none'
    }
  },
  palette: {
    type: "dark",
    primary: {
      main: amber[300]
    },
    secondary: {
      main: amber[900]
    },
    background: {
    //paper: 'black'
    }
  }
});

class App extends Component {
  constructor(props){
    super(props);
    this.api = new API(baseUri, this.reportError);
    this.state = {
      sidebarExpanded: false,
      transcriptText: 'Wellcome! \n This is the transcript..',
      pages: [],
      inspectors: []
    }
  }

  componentDidMount() {
    // this.openTranscript();
    // this.openInspectors();
    // this.openClassBrowser('Magnitude');
    // this.api.sendersOf('implementorsOf:')
    //   .then(methods => this.openMethodBrowser('#sendersOf:', methods));
    this.openWorkspace()
  }

  addPage(label, icon, component) {
    const page = {label: label, icon: icon, component: component};
    const pages = this.state.pages;
    pages.push(page);
    this.setState({pages: pages})
  }

  openTranscript() {
    const transcript = <Transcript
      classes={this.props.classes}
      text={this.state.transcriptText}
      />;
    this.addPage('Transcript', <TranscriptIcon className={this.props.classes.transcriptIcon} />, transcript);
  }

  openInspectors() {
    this.api.objects()
      .then(objects => {objects.forEach(o => this.openInspector(o))})
      .catch(error => {})
  }

  openClassBrowser(root) {
    const browser = <ClassBrowser
      api={this.api}
      classes={this.props.classes}
      root={root}
      onError={this.reportError}
      />;
    this.addPage(root, <ClassBrowserIcon className={this.props.classes.classBrowserIcon} />, browser);
  }

  openMethodBrowser(title, methods) {
    const browser = <MethodBrowser
      api={this.api}
      classes={this.props.classes}
      methods={methods}
      onError={this.reportError}
      />;
    this.addPage(title + '(' + methods.length + ')', <MethodBrowserIcon className={this.props.classes.methodBrowserIcon} />, browser);
  }

  openWorkspace() {
    const workspace = <Workspace
      api={this.api}
      classes={this.props.classes}
      //onEvaluate={this.openInspector}
      />;
    this.addPage('Workspace', <WorkspaceIcon className={this.props.classes.workspaceIcon} />, workspace);
  }

  openInspector(object) {
    console.log(this)
    console.log(object)
    const inspector = <Inspector
      api={this.api}
      key={object.id}
      baseUri={baseUri}
      classes={this.props.classes}
      root={object}
      onError={this.reportError}
      />;
    const inspectors = this.state.inspectors;
    inspectors.push(inspector);
    this.setState({inspectors: inspectors})
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

  reportError = (text) => {
    console.log(text)
    this.setState({transcriptText: this.state.transcriptText + '\n' + text})
  }
  
  render () {
    return (
      <ThemeProvider theme={theme}>
        <div className={this.props.classes.root}>
          <CssBaseline/>
          <Titlebar classes={this.props.classes} sidebarExpanded={this.state.sidebarExpanded} expandSidebar={this.expandSidebar} />
          <Sidebar
            classes={this.props.classes}
            expanded={this.state.sidebarExpanded}
            onClose={this.collapseSidebar}/>
          <main className={this.props.classes.content}>
            <div className={this.props.classes.appBarSpacer} />
            <Container className={this.props.classes.container}>
              <Grid container spacing={0}>
                <Grid item xs={12} md={9} lg={8}>
                    <TabControl classes={this.props.classes} pages={this.state.pages} onClose={this.closePage}/>
                </Grid>
                <Grid item xs={12} md={3} lg={4}>
                  {this.state.inspectors.map((inspector) => {return inspector})}
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
