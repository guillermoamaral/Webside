import { Component } from "react";
import { Box, Link, Typography } from "@mui/material";
import ToolContainerContext from "../ToolContainerContext";

class CodeInfo extends Component {
	static contextType = ToolContainerContext;

	render() {
		const {
			timestampLabel = "Modified on ",
			timestamp,
			authorLabel = " by ",
			author,
			packagename,
		} = this.props;
		return (
			<Box>
				{timestamp && (
					<Typography variant="caption">
						{timestampLabel + new Date(timestamp).toLocaleString()}
					</Typography>
				)}
				{author && (
					<Typography variant="caption">{authorLabel}</Typography>
				)}
				{author && (
					<Link
						variant="caption"
						href="#"
						onClick={(event) => {
							event.preventDefault();
							this.context.openChat(author);
						}}
					>
						{author}
					</Link>
				)}
				{(timestamp || author) && (
					<Typography variant="caption"> - </Typography>
				)}
				{packagename && (
					<Link
						variant="caption"
						href="#"
						onClick={(event) => {
							event.preventDefault();
							this.context.browsePackage(packagename);
						}}
					>
						{packagename}
					</Link>
				)}
			</Box>
		);
	}
}

export default CodeInfo;
