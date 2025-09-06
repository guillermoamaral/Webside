import Tool from "./Tool";
import { Box, Typography } from "@mui/material";
import MarkdownView from "../parts/MarkdownView";
import axios from "axios";
import { VERSION } from "../../config";

class ReleaseNotes extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			title: "Release notes",
			version: "",
			notes: "",
		};
	}

	async componentDidMount() {
		const json = await this.fetchReleaseNotes(VERSION);
		this.setState({ version: json.tag, notes: json.markdown });
	}

	async fetchReleaseNotes(tag = null) {
		const base =
			"https://api.github.com/repos/guillermoamaral/Webside/releases";
		const url = tag ? `${base}/tags/${tag}` : `${base}/latest`;

		try {
			const res = await axios.get(url, {
				headers: {
					Accept: "application/vnd.github+json",
				},
			});

			const data = res.data;
			return {
				tag: data.tag_name,
				title: data.name || data.tag_name,
				markdown: data.body,
				url: data.html_url,
				publishedAt: data.published_at,
			};
		} catch (e) {
			console.error("Error fetching release notes:", e.message);
			return null;
		}
	}

	render() {
		const { title, version, notes } = this.state;
		return (
			<Box
				height="100%"
				width="100%"
				display="flex"
				flexDirection="column"
			>
				<Typography variant="h6">
					{title + " of version " + version + ""}
				</Typography>
				<MarkdownView source={notes} />
			</Box>
		);
	}
}

export default ReleaseNotes;
