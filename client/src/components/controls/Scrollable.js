import React from "react";
import Scrollbar from "react-scrollbars-custom";

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
		return (
			<Scrollbar style={{ width: "100%", height: "100%"}}>
				{this.props.children}
			</Scrollbar>
		);
	}
}

export default Scrollable;
