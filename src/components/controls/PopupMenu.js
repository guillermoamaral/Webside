import React, { Component } from "react";
import { Menu, MenuItem, Divider } from "@mui/material";
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
				if (option.suboptions) {
					return (
						<NestedMenuItem
							key={option.label}
							label={option.label}
							parentMenuOpen={this.props.open}
							style={{ paddingTop: 0, paddingBottom: 0 }}
						>
							{this.createItems(option.suboptions)}
						</NestedMenuItem>
					);
				} else {
					const enabled = this.getItemEnabled(option);
					return (
						<MenuItem
							key={option.label}
							id={option.id}
							onClick={(event) => this.itemClicked(event, option)}
							style={{ paddingTop: 0, paddingBottom: 0 }}
							disabled={!enabled}
						>
							{option.label}
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
		if (typeof enabled === "boolean") {
			return enabled;
		}
		if (enabled) {
			return enabled(option);
		}
		return true;
	}

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
