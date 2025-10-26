import { styled } from "@mui/material/styles";
import Drawer from "@mui/material/Drawer";

const drawerWidth = 240;

const openedMixin = (theme) => ({
	width: drawerWidth,
	transition: theme.transitions.create("width", {
		easing: theme.transitions.easing.sharp,
		duration: theme.transitions.duration.enteringScreen,
	}),
	overflowX: "hidden",
});

const closedMixin = (theme) => ({
	transition: theme.transitions.create("width", {
		easing: theme.transitions.easing.sharp,
		duration: theme.transitions.duration.leavingScreen,
	}),
	overflowX: "hidden",
	width: `calc(${theme.spacing(7)} + 1px)`,
	[theme.breakpoints.up("sm")]: {
		width: `calc(${theme.spacing(8)} + 1px)`,
	},
});

const StyledDrawer = styled(Drawer, {
	shouldForwardProp: (prop) => prop !== "open",
})(({ theme, open }) => ({
	width: drawerWidth,
	flexShrink: 0,
	whiteSpace: "nowrap",
	boxSizing: "border-box",
	"& .MuiDrawer-paper": {
		top: '0px',
		height: '100%',
	},
	...(open && {
		...openedMixin(theme),
		"& .MuiDrawer-paper": {
			...openedMixin(theme),
			top: '0px',
			height: '100%',
		},
	}),
	...(!open && {
		...closedMixin(theme),
		"& .MuiDrawer-paper": {
			...closedMixin(theme),
			top: '0px',
			height: '100%',
		},
	}),
}));

export default StyledDrawer;
