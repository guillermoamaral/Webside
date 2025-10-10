import React, { PureComponent } from "react";
import Split from "@uiw/react-split";

let styleElement = null;
if (typeof document !== 'undefined' && !styleElement) {
	styleElement = document.createElement("style");
	styleElement.textContent = `
		.w-split-bar::before,
		.w-split-bar::after {
			display: none !important;
		}
		
		.w-split-bar {
			background: transparent !important;
			box-shadow: none !important;
			transition: all 0.2s ease !important;
		}
		
		.w-split-bar:hover {
			background: rgba(0, 122, 204, 0.5) !important;
			box-shadow: none !important;
		}
		
		.w-split-horizontal > .w-split-bar {
			width: 3px !important;
		}
		
		.w-split-horizontal > .w-split-bar:hover {
			width: 3px !important;
		}
		
		.w-split-vertical > .w-split-bar {
			height: 3px !important;
		}
		
		.w-split-vertical > .w-split-bar:hover {
			height: 3px !important;
		}
	`;
	document.head.appendChild(styleElement);
}

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
                                backgroundColor: "transparent",
                            }}
                        >
                            <div
                                onMouseDown={onMouseDown}
                                style={{
                                    backgroundColor: "transparent",
                                    boxShadow: "none",
                                }}
                            />
                        </div>
                    );
                }}
                style={{
                    height: "100%",
                    width: "100%",
                    boxShadow: "none",
                    background: "transparent",
                }}
            >
                {this.props.children}
            </Split>
        );
    }
}

export default CustomSplit;
