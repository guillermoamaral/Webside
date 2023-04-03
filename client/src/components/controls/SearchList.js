import React, { Component } from "react";
import PropTypes from "prop-types";
import TextField from "@mui/material/TextField";
import Autocomplete from "@mui/lab/Autocomplete";
import useMediaQuery from "@mui/material/useMediaQuery";
import ListSubheader from "@mui/material/ListSubheader";
import { useTheme } from "@mui/material/styles";
import { VariableSizeList } from "react-window";
import { Typography } from "@mui/material";
//import Scrollbar from "react-scrollbars-custom";

const LISTBOX_PADDING = 0; // px

function renderRow(props) {
	const { data, index, style } = props;
	return React.cloneElement(data[index], {
		style: {
			...style,
			top: style.top + LISTBOX_PADDING,
		},
	});
}

const OuterElementContext = React.createContext({});
const OuterElementType = React.forwardRef((props, ref) => {
	const outerProps = React.useContext(OuterElementContext);
	return <div ref={ref} {...props} {...outerProps} />;
});

function useResetCache(data) {
	const ref = React.useRef(null);
	React.useEffect(() => {
		if (ref.current != null) {
			ref.current.resetAfterIndex(0, true);
		}
	}, [data]);
	return ref;
}

// Adapter for react-window
const ListboxComponent = React.forwardRef(function ListboxComponent(
	props,
	ref
) {
	const { children, ...other } = props;
	const itemData = React.Children.toArray(children);
	const theme = useTheme();
	const smUp = useMediaQuery(theme.breakpoints.up("sm"), { noSsr: true });
	const itemCount = itemData.length;
	const itemSize = smUp ? 36 : 48;

	const getChildSize = (child) => {
		if (React.isValidElement(child) && child.type === ListSubheader) {
			return 48;
		}
		return itemSize;
	};

	const getHeight = () => {
		if (itemCount > 8) {
			return 8 * itemSize;
		}
		return itemData.map(getChildSize).reduce((a, b) => a + b, 0);
	};

	const gridRef = useResetCache(itemCount);
	return (
		<div ref={ref}>
			<OuterElementContext.Provider value={other}>
				<VariableSizeList
					itemData={itemData}
					height={getHeight() + 2 * LISTBOX_PADDING}
					width="100%"
					ref={gridRef}
					outerElementType={OuterElementType}
					innerElementType="ul"
					itemSize={(index) => getChildSize(itemData[index])}
					overscanCount={5}
					itemCount={itemCount}
				>
					{renderRow}
				</VariableSizeList>
			</OuterElementContext.Provider>
		</div>
	);
});

ListboxComponent.propTypes = {
	children: PropTypes.node,
};

class SearchList extends Component {
	valueChanged(value) {
		if (this.props.onChange) {
			this.props.onChange(value);
		}
	}

	render() {
		return (
			<Autocomplete
				popupIcon={null}
				autoHighlight
				clearOnEscape
				id="autocomplete"
				ListboxComponent={ListboxComponent}
				renderGroup={(params) => [
					<ListSubheader key={params.key} component="div">
						{params.group}
					</ListSubheader>,
					params.children,
				]}
				vallue="pirulo"
				onChange={(event, value) => {
					this.valueChanged(value);
				}}
				options={this.props.options}
				renderInput={(params) => (
					<TextField
						{...params}
						size="small"
						variant="outlined"
						label="Search..."
					/>
				)}
				renderOption={(option) => <Typography noWrap>{option}</Typography>}
			/>
		);
	}
}

export default SearchList;
