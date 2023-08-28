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
import SearchIcon from "@mui/icons-material/SearchRounded";

class SearchList2 extends React.Component {
	constructor(props) {
		super(props);
		this.typingTimer = null;
		this.state = {
			value: props.value || "",
			suggestions: [],
		};
	}

	suggestionLimit() {
		return this.props.suggestionLimit || 5;
	}

	async getSuggestions(value) {
		const options = this.props.options;
		if (!options) return [];
		const limit = this.suggestionLimit();
		if (typeof options === "function") {
			const retrieved = await options(value);
			return retrieved.slice(0, Math.min(retrieved.length, limit));
		}
		const target = value.trim().toLowerCase();
		const length = target.length;
		if (length === 0) return [];
		let count = 0;
		return options.filter((option) => {
			const keep =
				count < limit &&
				option &&
				option.toLowerCase().slice(0, length) === target;
			if (keep) {
				count += 1;
			}
			return keep;
		});
	}

	suggestionsFetchRequested = async ({ value }) => {
		clearTimeout(this.typingTimer);
		this.typingTimer = setTimeout(async () => {
			const suggestions = await this.getSuggestions(value);
			this.setState({ suggestions: suggestions });
		}, 500);
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
				onKeyDown={(event) => {
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
		const { backColor } = this.props;
		return (
			<Autosuggest
				theme={{
					container: {
						position: "relative",
					},
					suggestion: {
						display: "block",
					},
					suggestionsContainerOpen: {
						display: "block",
						position: "absolute",
						top: 51,
						width: "100%",
						border: "2px solid #c4c4c41f",
						backgroundColor: backColor,
						zIndex: 2,
					},
					suggestionsList: {
						margin: 0,
						padding: 0,
						listStyleType: "none",
					},
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

export default SearchList2;
