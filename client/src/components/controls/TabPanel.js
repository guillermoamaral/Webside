import React, { Component } from "react";
import { Box } from "@material-ui/core";
import PropTypes from "prop-types";

class TabPanel extends Component {
	render() {
		const { id, children, visible, ...other } = this.props;
		return (
			<div role="tabpanel" hidden={!visible} id={id} {...other}>
				<Box p={1} style={{ height: "100%" }}>
					{children}
				</Box>
			</div>
		);
	}
}

TabPanel.propTypes = {
	children: PropTypes.node,
	id: PropTypes.any.isRequired,
	visible: PropTypes.any.isRequired,
};

export default TabPanel;
