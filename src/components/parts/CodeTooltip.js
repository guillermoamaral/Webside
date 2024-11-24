import React, { Component } from "react";
import {
	Link,
	Card,
	CardContent,
	CardActions,
	Button,
	Typography,
	Paper,
} from "@mui/material";
import { ide } from "../IDE";
import CodeEditor from "./CodeEditor";

class CodeTooltip extends Component {
	render() {
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const color = mode.get("primaryColor");
		const background = mode.get("background");
		const { title, titleAction, description, code, actions } = this.props;
		return (
			<Card
				sx={{
					minWidth: 200,
					maxWidth: 600,
					border: 1,
					borderColor: "grey.500",
					background: background,
				}}
			>
				<CardContent>
					<Link
						component="button"
						variant="body1"
						onClick={(e) => {
							if (titleAction) titleAction(title);
						}}
						color={color}
					>
						{title}
					</Link>
					{description && (
						<Typography
							variant="body2"
							color={mode.get("primaryText")}
						>
							{description || ""}
						</Typography>
					)}
					{code && (
						<Paper
							variant="outlined"
							sx={{
								minWidth: 400,
								width: "100%",
								height: 150,
								background: background,
							}}
						>
							<CodeEditor source={code} readOnly noTooltips />
						</Paper>
					)}
				</CardContent>
				{actions && (
					<CardActions>
						{actions.map((action, i) => {
							return (
								<Button
									size="small"
									sx={{
										textTransform: "none",
										color: color,
									}}
									key={"tipAction" + i}
									onClick={() => action.handler(title)}
								>
									{action.label}
								</Button>
							);
						})}
					</CardActions>
				)}
			</Card>
		);
	}
}

export default CodeTooltip;
