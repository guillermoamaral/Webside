import { SvgIcon } from "@mui/material";

export default function ObjectsIcon(props) {
	return (
		<SvgIcon color={props.color}>
			<path
				fill="currentColor"
				d="M6 21q-1.65 0-2.825-1.175T2 17q0-1.65 1.175-2.825T6 13q1.65 0 2.825 1.175T10 17q0 1.65-1.175 2.825T6 21Zm12 0q-1.65 0-2.825-1.175T14 17q0-1.65 1.175-2.825T18 13q1.65 0 2.825 1.175T22 17q0 1.65-1.175 2.825T18 21Zm-6-10q-1.65 0-2.825-1.175T8 7q0-1.65 1.175-2.825T12 3q1.65 0 2.825 1.175T16 7q0 1.65-1.175 2.825T12 11Z"
			/>
		</SvgIcon>
	);
}
