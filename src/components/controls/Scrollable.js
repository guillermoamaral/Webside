import React from "react";
import Scrollbar from "react-scrollbars-custom";
import { Box } from "@mui/material";

class Scrollable extends React.PureComponent {
	// trackV = ({ style, ...props }) => (
	//   <div className="track track-vertical" style={{...style, backgroundColor: 'red'}} {...props} />
	// );
	// thumbV = ({ style, ...props }) => (
	//   <div className="thumb thumb-vertical" style={{...style}} {...props} />
	// );
	// trackH = ({ style, ...props }) => (
	//   <div className="track track-horizontal" style={{...style}} {...props} />
	// );
	// thumbH = ({ style, ...props }) => (
	//   <div className="thumb thumb-horizontal" style={{...style}} {...props} />
	// );
	render() {
		// const sProps = {
		//     renderTrackHorizontal: this.trackH,
		//     renderThumbHorizontal: this.thumbH,
		//     renderThumbVertical: this.thumbV,
		//     renderTrackVertical: this.trackV,
		//     autoHide: false}
		//return <Scrollbar {...sProps}>{this.props.children}</Scrollbar>;
		const { disabled, children, ...rest } = this.props;
		if (disabled) {
			return (
				<Box
					sx={{
						width: "100%",
						height: "100%",
						overflow: "hidden",
					}}
					{...rest}
				>
					{children}
				</Box>
			);
		}
		return (
			<Scrollbar
				style={{ width: "100%", height: "100%" }}
				contentProps={{ style: { padding: 0, margin: 0 } }}
				wrapperProps={{ style: { padding: 0, margin: 0 } }}
				{...rest}
			>
				{children}
			</Scrollbar>
		);
	}
}

export default Scrollable;
