import React from "react";
import Autosuggest from "react-autosuggest";
import match from "autosuggest-highlight/match";
import parse from "autosuggest-highlight/parse";
import {
	TextField,
	MenuItem,
	Box,
	Typography,
	InputAdornment,
} from "@mui/material";
// Must get rid of withStyles
import { withStyles } from "@mui/styles";
import { styled } from "@mui/material/styles";
import SearchIcon from "@mui/icons-material/SearchRounded";

const styles = (theme) => ({
	container: {
		flexGrow: 1,
		position: "relative",
		//height: 250,
	},
	suggestion: {
		display: "block",
	},
	suggestionsList: {
		margin: 0,
		padding: 0,
		listStyleType: "none",
	},
});

const suggestionLimit = 5;

// const StyledAutosuggest = styled(Autosuggest)(({ theme }) => ({
// 	".SearchList-container-1": {
// 		flexGrow: 1,
// 		position: "relative",
// 		height: 250,
// 	},
// 	".SearchList-suggestion-1": {
// 		display: "block",
// 	},
// 	".SearchList-suggestionsList-1": {
// 		margin: 0,
// 		padding: 0,
// 		listStyleType: "none",
// 	},
// }));

class SearchList2 extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			value: props.value || "",
			suggestions: [],
		};
	}

	filterSuggestions(value) {
		const inputValue = value.trim().toLowerCase();
		const inputLength = inputValue.length;
		let count = 0;
		return inputLength === 0 || !this.props.options
			? []
			: this.props.options.filter((option) => {
					const keep =
						count < suggestionLimit &&
						option &&
						option.toLowerCase().slice(0, inputLength) ===
							inputValue;
					if (keep) {
						count += 1;
					}
					return keep;
			  });
	}

	suggestionsFetchRequested = ({ value }) => {
		this.setState({ suggestions: this.filterSuggestions(value) });
	};

	suggestionsClearRequested = () => {
		this.setState({ suggestions: [] });
	};

	inputChanged = (event, { newValue }) => {
		//const handler = this.props.onChange;
		this.setState({ value: newValue });
	};

	valueChanged = (value) => {
		if (this.props.onChange) {
			this.props.onChange(value);
		}
	};

	renderInput = (inputProps) => {
		const { ref, ...other } = inputProps;
		return (
			<TextField
				fullWidth
				size="small"
				variant="outlined"
				InputProps={{ inputRef: ref, ...other }}
				autoFocus
				onKeyPress={(event) => {
					if (
						event.key === "Enter" &&
						this.state.suggestions.length > 0
					) {
						event.preventDefault();
						this.valueChanged(this.state.suggestions[0]);
					}
				}}
			/>
		);
	};

	renderSuggestionsContainer = (options) => {
		const { containerProps, children } = options;
		return (
			<Box {...containerProps} zIndex="tooltip">
				{children}
			</Box>
		);
	};

	renderSuggestion = (suggestion, { query, isHighlighted }) => {
		const matches = match(suggestion, query);
		const parts = parse(suggestion, matches);
		return (
			<MenuItem selected={isHighlighted} component="div">
				<Typography component="div">
					{parts.map((part, index) => {
						return part.highlight ? (
							<Box component="strong" key={index}>
								{part.text}
							</Box>
						) : (
							<Box component="span" key={index}>
								{part.text}
							</Box>
						);
					})}
				</Typography>
			</MenuItem>
		);
	};

	render() {
		const { classes } = this.props;
		return (
			<Autosuggest
				// theme={{
				// 	container: "SearchList-container-1",
				// 	suggestionsList: "SearchList-suggestionsList-1",
				// 	suggestion: "SearchList-suggestion-1",
				// }}
				theme={{
					container: classes.container,
					suggestionsList: classes.suggestionsList,
					suggestion: classes.suggestion,
				}}
				renderInputComponent={this.renderInput}
				suggestions={this.state.suggestions}
				onSuggestionsFetchRequested={this.suggestionsFetchRequested}
				onSuggestionsClearRequested={this.suggestionsClearRequested}
				renderSuggestionsContainer={this.renderSuggestionsContainer}
				getSuggestionValue={(s) => s}
				renderSuggestion={this.renderSuggestion}
				onSuggestionSelected={(event, suggestion) =>
					this.valueChanged(suggestion.suggestion)
				}
				itemProps={{ style: { listStyleType: "none" } }}
				inputProps={{
					placeholder: "Search...",
					value: this.state.value,
					onChange: this.inputChanged,
					startAdornment: (
						<InputAdornment position="start">
							<SearchIcon />
						</InputAdornment>
					),
				}}
				// ref={(autosuggest) => {
				// 	if (autosuggest) {
				// 		autosuggest.input.focus();
				// 	}
				// }}
			/>
		);
	}
}

export default withStyles(styles)(SearchList2);

//export default SearchList2;
