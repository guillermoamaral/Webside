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
    paddingTop: "12px",
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
})

export default styles;