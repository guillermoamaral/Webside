import React, { Component } from "react";
import { IconButton, Box } from "@mui/material";
import CloseIcon from "@mui/icons-material/Close";

class TabLabel extends Component {
	constructor(props) {
		super(props);
		this.state = { label: props.label };
	}

	changeLabel(label) {
		this.setState({ label: label });
	}

	render() {
		const { index, icon, noClose, onClose } = this.props;
		const { label } = this.state;
		return (
			<Box
				display="flex"
				flexWrap="nowrap"
				alignItems="center"
				justifyContent="center"
			>
				<Box pt={1}>{icon}</Box>
				<Box pl={1} pr={1}>
					{label}
				</Box>
				<Box>
					{!noClose && (
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
			</Box>
		);
	}
}

export default TabLabel;
