import { randomNumberBetween } from "@mui/x-data-grid/utils/utils";
import MonacoDiffEditor from "../parts/MonacoDiffEditor";
import Tool from "./Tool";
import { Box, Button } from "@mui/material";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			index: 1,
			data: [
				{
					left: "a\nb\nc",
					right: "a\nb\nd",
				},
				{
					left: "ab\nc",
					right: "abc\nd",
				},
			],
		};
	}

	render() {
		const { data, index } = this.state;
		return (
			<Box
				display="flex"
				flexDirection="column"
				justifyItems={"center"}
				height="100%"
			>
				<Box>
					<Button
						onClick={() =>
							this.setState({
								index: index === 1 ? 0 : 1, //Math.floor(Math.random() * data.length),
							})
						}
					>
						{"Change sources -> " + index}
					</Button>
				</Box>
				<Box flexGrow={1} width="100%">
					<MonacoDiffEditor
						leftSource={data[index].left}
						rightSource={data[index].right}
					/>
				</Box>
			</Box>
		);
	}
}

export default POC;
