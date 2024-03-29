import React, { PureComponent } from "react";
import Split from "@uiw/react-split";

class CustomSplit extends PureComponent {
	render() {
		return (
			<Split
				{...this.props}
				//Not sure if this is the best option... perhaps removing this customization produces better results
				renderBar={({ onMouseDown, ...props }) => {
					return (
						<div
							{...props}
							style={{
								boxShadow: "none",
								background: "transparent",
							}}
						>
							<div
								onMouseDown={onMouseDown}
								style={{
									backgroundColor: "default",
									boxShadow: "none",
								}}
							/>
						</div>
					);
				}}
				style={{
					height: "100%",
					width: "100%",
				}}
			>
				{this.props.children}
			</Split>
		);
	}
}

export default CustomSplit;
