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
			mouseHovering: false,
		};
	}

	changeLabel(label) {
		this.setState({ label: label });
	}

	openMenu = (event) => {
		event.preventDefault();
		event.stopPropagation();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	menuOptions() {
		const options = [];
		const showCloseOptions =
			this.props.showCloseOptions === undefined
				? true
				: this.props.showCloseOptions;
		if (showCloseOptions) {
			options.push(
				...[
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
				]
			);
		}
		// Disabled for the moment
		// {
		// 	label: "Split",
		// 	action: this.splitTab,
		// },
		return options;
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
		const {
			index,
			icon,
			onClose,
			showCloseOptions = true,
			selected = false,
		} = this.props;
		const { menuOpen, menuPosition, mouseHovering } = this.state;
		const text = this.visibleLabel();
		const menuOptions = this.menuOptions();
		return (
			<Box
				display="flex"
				flexWrap="nowrap"
				alignItems="center"
				justifyContent="center"
				onContextMenu={this.openMenu}
				onMouseEnter={() => this.setState({ mouseHovering: true })}
				onMouseLeave={() => this.setState({ mouseHovering: false })}
			>
				{icon && <Box pr={1}>{icon}</Box>}
				<Box>{text}</Box>
				<Box>
					{showCloseOptions && (
						<IconButton
							onClick={(event) => {
								onClose(event, index);
							}}
							id={index}
							value={index}
							size="small"
							sx={{
								opacity: selected ? 1 : 0.5,
								visibility:
									mouseHovering || selected
										? "visible"
										: "hidden",
								transition: "opacity 0.2s ease-in-out",
							}}
						>
							<CloseIcon
								fontSize="small"
								id={index}
								value={index}
							/>
						</IconButton>
					)}
				</Box>
				{menuOptions && menuOptions.length > 0 && (
					<PopupMenu
						options={menuOptions}
						open={menuOpen}
						position={menuPosition}
						onClose={this.closeMenu}
					/>
				)}
			</Box>
		);
	}
}

export default TabLabel;
