import { AppBar } from "@mui/material";
import { styled } from "@mui/material/styles";

const drawerWidth = 240;

const StyledAppBar = styled(AppBar, {
	shouldForwardProp: (prop) => prop !== "open" && prop !== "isElectron",
})(({ theme, open, isElectron }) => ({
	zIndex: theme.zIndex.drawer + 1,
	transition: theme.transitions.create(["width", "margin"], {
		easing: theme.transitions.easing.sharp,
		duration: theme.transitions.duration.leavingScreen,
	}),
	...(open && {
		marginLeft: drawerWidth,
		width: `calc(100% - ${drawerWidth}px)`,
		transition: theme.transitions.create(["width", "margin"], {
			easing: theme.transitions.easing.sharp,
			duration: theme.transitions.duration.enteringScreen,
		}),
	}),
	...(isElectron && {
		'& .MuiToolbar-root': {
			paddingRight: 1,
		},
		WebkitAppRegion: 'drag',
		appRegion: 'drag',
	}),
}));

export default StyledAppBar;
