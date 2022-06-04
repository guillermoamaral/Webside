import React, { Component } from "react";
import { Menu, MenuItem, Divider } from "@material-ui/core";
import NestedMenuItem from "material-ui-nested-menu-item";

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
					return (
						<MenuItem
							key={option.label}
							id={option.id}
							onClick={(event) => this.itemClicked(event, option)}
							style={{ paddingTop: 0, paddingBottom: 0 }}
						>
							{option.label}
						</MenuItem>
					);
				}
			}
		});
	};

	position() {
		if (this.props.position && this.props.position.x && this.props.position.y) {
			return { left: this.props.position.x, top: this.props.position.y };
		}
	}

	itemClicked = (event, option) => {
		event.stopPropagation();
		this.close();
		const handler = this.props.onOptionClick;
		if (handler) {
			handler(option);
		} else {
			option.action();
		}
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
