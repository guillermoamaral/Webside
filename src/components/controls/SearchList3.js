import React, { Component } from "react";
import PropTypes from "prop-types";
import TextField from "@mui/material/TextField";
import Autocomplete, { autocompleteClasses } from "@mui/material/Autocomplete";
import useMediaQuery from "@mui/material/useMediaQuery";
import ListSubheader from "@mui/material/ListSubheader";
import Popper from "@mui/material/Popper";
import { useTheme, styled } from "@mui/material/styles";
import { VariableSizeList } from "react-window";
import Typography from "@mui/material/Typography";

const LISTBOX_PADDING = 8; // px

function renderRow(props) {
	const { data, index, style } = props;
	const dataSet = data[index];
	const inlineStyle = {
		...style,
		top: style.top + LISTBOX_PADDING,
	};

	if (dataSet.hasOwnProperty("group")) {
		return (
			<ListSubheader
				key={dataSet.key}
				component="div"
				style={inlineStyle}
			>
				{dataSet.group}
			</ListSubheader>
		);
	}

	return (
		<Typography component="li" {...dataSet[0]} noWrap style={inlineStyle}>
			{dataSet[1]}
		</Typography>
	);
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
	const itemData = [];
	children.forEach((item) => {
		itemData.push(item);
		itemData.push(...(item.children || []));
	});

	const theme = useTheme();
	const smUp = useMediaQuery(theme.breakpoints.up("sm"), {
		noSsr: true,
	});
	const itemCount = itemData.length;
	const itemSize = smUp ? 36 : 48;

	const getChildSize = (child) => {
		if (child.hasOwnProperty("group")) {
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

const StyledPopper = styled(Popper)({
	[`& .${autocompleteClasses.listbox}`]: {
		boxSizing: "border-box",
		"& ul": {
			padding: 0,
			margin: 0,
		},
	},
});

class SearchList3 extends Component {
	render() {
		return (
			<Autocomplete
				id="SearchList"
				size="small"
				//sx={{ width: 300 }}
				disableListWrap
				PopperComponent={StyledPopper}
				ListboxComponent={ListboxComponent}
				options={this.props.options || []}
				//groupBy={(option) => option[0].toUpperCase()}
				renderInput={(params) => (
					<TextField
						{...params}
						label={this.props.label || "Search..."}
					/>
				)}
				onChange={(event, value) => {
					if (this.props.onChange) {
						this.props.onChange(value);
					}
				}}
				renderOption={(props, option, state) => [
					props,
					option,
					state.index,
				]}
				// TODO: Post React 18 update - validate this conversion, look like a hidden bug
				//renderGroup={(params) => params}
			/>
		);
	}
}

export default SearchList3;
