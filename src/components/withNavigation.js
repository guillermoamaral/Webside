import React from "react";
import { useNavigate } from "react-router-dom";
import { useLocation } from "react-router-dom";

export const withNavigation = (Component) => {
	return (props) => (
		<Component
			{...props}
			navigate={useNavigate()}
			location={useLocation()}
		/>
	);
};
