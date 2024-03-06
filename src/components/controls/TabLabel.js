import React, { Component } from "react";
import { IconButton, Box } from "@mui/material";
import CloseIcon from "@mui/icons-material/Close";
import PopupMenu from "./PopupMenu";

class TabLabel extends Component {
	constructor(props) {
		super(props);
		this.state = {
			label: props.label,
			menuOpen: false,
			menuPosition: { x: null, y: null },
		};
	}

	changeLabel(label) {
		this.setState({ label: label });
	}

	openMenu = (event) => {
		event.preventDefault();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	menuOptions() {
		return [
			{
				label: "Close",
				action: this.closeTab,
			},
			{
				label: "Close all",
				action: this.closeAllTabs,
			},
			{
				label: "Close others",
				action: this.closeOtherTabs,
			},
			// {	//disabled for the moment
			// 	label: "Split",
			// 	action: this.splitTab,
			// },
		];
	}

	splitTab = () => {
		this.props.onSplit(this.props.index);
	};

	closeTab = () => {
		this.props.onClose(null, this.props.index);
	};

	closeAllTabs = () => {
		this.props.onCloseAll();
	};

	closeOtherTabs = () => {
		this.props.onCloseOthers(this.props.index);
	};

	visibleLabel() {
		const label = this.state.label;
		const max = 40;
		return label.length > max ? label.substr(0, max - 1) + "…" : label;
	}

	render() {
		const { index, icon, onClose } = this.props;
		let showClose =
			this.props.showClose === undefined ? true : this.props.showClose;
		const { menuOpen, menuPosition } = this.state;
		const text = this.visibleLabel();
		return (
			<Box
				display="flex"
				flexWrap="nowrap"
				alignItems="center"
				justifyContent="center"
				onContextMenu={(event) => {
					this.openMenu(event);
				}}
			>
				<Box pt={1}>{icon}</Box>
				<Box pl={1} pr={1} pt={1}>
					{text}
				</Box>
				<Box pt={1}>
					{showClose && (
						<IconButton
							onClick={(event) => {
								onClose(event, index);
							}}
							id={index}
							value={index}
							size="small"
						>
							<CloseIcon
								fontSize="small"
								id={index}
								value={index}
							/>
						</IconButton>
					)}
				</Box>
				<PopupMenu
					options={this.menuOptions()}
					open={menuOpen}
					position={menuPosition}
					onClose={this.closeMenu}
				/>
			</Box>
		);
	}
}

export default TabLabel;
