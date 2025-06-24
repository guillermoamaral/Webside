import React, { Component } from "react";
import { Menu, MenuItem, Divider, Box, Typography } from "@mui/material";
import { NestedMenuItem } from "mui-nested-menu";

class PopupMenu extends Component {
	createItems = (options) => {
		if (!options) {
			return [];
		}
		return options.map((option, index) => {
			if (!option) {
				return <Divider key={"divider-" + index} />;
			} else {
				const enabled = this.getItemEnabled(option);
				const style = this.getItemStyle(option);
				const weight = this.getItemWeight(option);
				if (option.suboptions) {
					return (
						<NestedMenuItem
							key={option.label}
							renderLabel={() => (
								<Box
									sx={{
										pl: 1.5,
										pt: 0,
										pb: 0,
										display: "flex",
										justifyContent: "space-between",
										alignItems: "baseline",
										fontStyle: style,
										fontWeight: weight,
										width: "100%",
									}}
								>
									<Typography variant="body2">
										{option.label}
									</Typography>
									{option.shortcut && (
										<Typography
											variant="caption"
											sx={{ ml: 1, opacity: 0.6 }}
										>
											{"(" + option.shortcut + ")"}
										</Typography>
									)}
								</Box>
							)}
							parentMenuOpen={this.props.open}
							sx={{ pt: 0, pb: 0 }}
						>
							{this.createItems(option.suboptions)}
						</NestedMenuItem>
					);
				} else {
					return (
						<MenuItem
							key={option.label}
							id={option.id}
							onClick={(event) => this.itemClicked(event, option)}
							sx={{ pt: 0, pb: 0 }}
							disabled={!enabled}
						>
							<Box
								sx={{
									display: "flex",
									justifyContent: "space-between",
									alignItems: "baseline",
									fontStyle: style,
									fontWeight: weight,
									width: "100%",
								}}
							>
								<Typography variant="body2">
									{option.label}
								</Typography>
								{option.shortcut && (
									<Typography
										variant="caption"
										sx={{ ml: 1, opacity: 0.6 }}
									>
										{"(" + option.shortcut + ")"}
									</Typography>
								)}
							</Box>
						</MenuItem>
					);
				}
			}
		});
	};

	position() {
		if (
			this.props.position &&
			this.props.position.x &&
			this.props.position.y
		) {
			return { left: this.props.position.x, top: this.props.position.y };
		}
	}

	itemClicked = (event, option) => {
		event.stopPropagation();
		this.close();
		try {
			if (this.props.onOptionClick) {
				this.props.onOptionClick(option);
			} else {
				option.action();
			}
		} catch (error) {
			console.log(error);
		}
	};

	getItemEnabled(option) {
		const enabled = this.props.onOptionEnable;
		if (typeof enabled === "boolean") return enabled;
		if (enabled) return enabled(option);
		return true;
	}

	getItemStyle = (option) => {
		const style = option.style;
		if (!style) return "normal";
		if (typeof style === "string") return style;
		return style(option);
	};

	getItemWeight = (option) => {
		const weight = option.weight;
		if (!weight) return "normal";
		if (typeof weight === "string") return weight;
		return weight(option);
	};

	close = () => {
		const handler = this.props.onClose;
		if (handler) {
			handler.bind(this);
			handler();
		}
	};

	render() {
		return (
			<Menu
				keepMounted
				open={this.props.open}
				onClose={this.close}
				anchorReference="anchorPosition"
				anchorPosition={this.position()}
			>
				{this.createItems(this.props.options)}
			</Menu>
		);
	}
}

export default PopupMenu;
