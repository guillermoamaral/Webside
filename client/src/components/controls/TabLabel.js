import React, { Component } from "react";
import { IconButton, Box } from "@material-ui/core";
import CloseIcon from "@material-ui/icons/Close";

class TabLabel extends Component {
	render() {
		const { index, icon, label, onClose } = this.props;
		console.log("rendering tab label");
		return (
			<Box
				display="flex"
				flexWrap="nowrap"
				alignItems="center"
				justifyContent="center"
			>
				<Box pt={1} pr={1}>
					{icon}
				</Box>
				<Box pr={1}>{label}</Box>
				<Box>
					<IconButton
						onClick={(event) => {
							onClose(event, index);
						}}
						id={index}
						value={index}
						size="small"
					>
						<CloseIcon fontSize="small" id={index} value={index} />
					</IconButton>
				</Box>
			</Box>
		);
	}
}

export default TabLabel;
