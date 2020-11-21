import React from 'react';
import Autosuggest from 'react-autosuggest';
import match from 'autosuggest-highlight/match';
import parse from 'autosuggest-highlight/parse';
import { TextField, Paper, MenuItem } from '@material-ui/core';

class SearchList2 extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            value: '',
            suggestions: [],
        }
    }
    
    filterSuggestions(value) {
        const inputValue = value.trim().toLowerCase();
        const inputLength = inputValue.length;
        let count = 0;
        return inputLength === 0
            ? []
            : this.props.options.filter(o => {
                const keep = count < 5 && o.toLowerCase().slice(0, inputLength) === inputValue;
                if (keep) {count += 1}
                return keep;
            })
    }

    suggestionsFetchRequested = ({value}) => {
        this.setState({suggestions: this.filterSuggestions(value)});
    }

    suggestionsClearRequested = () => {
        this.setState({suggestions: []})
    }

    inputChanged = (event, {newValue}) => {
        const handler = this.props.onChange;
        this.setState({value: newValue});
    }

    valueChanged = (value) => {
        const handler = this.props.onChange;
        if (handler) {handler(value)};
    }

    renderInput = (inputProps) => {
        const {ref, ...other} = inputProps;
        return (
            <TextField
                fullWidth
                size="small"
                variant="outlined"
                InputProps={{inputRef: ref, ...other}}/>
        )
    }

    renderSuggestionsContainer = (options) => {
        const {containerProps, children} = options;
        return (
            <Paper {...containerProps} square>
                {children}
            </Paper>
        )
    }

    renderSuggestion = (suggestion, {query, isHighlighted}) => {
        const matches = match(suggestion, query);
        const parts = parse(suggestion, matches);
        return (
            <MenuItem selected={isHighlighted} component="div" onClick={event => this.valueChanged(suggestion)} style={{listStyleType: 'none'}}>
                <div>
                    {parts.map((part, index) => {
                        return part.highlight ? (
                            <span key={String(index)} style={{fontWeight: 300}}>
                            {part.text}
                            </span>
                        ) : (
                            <strong key={String(index)} style={{fontWeight: 500}}>
                            {part.text}
                            </strong>
                        )})}
                </div>
            </MenuItem>
        )
    }

    render() {
        return (
            <Autosuggest
                renderInputComponent={this.renderInput}
                suggestions={this.state.suggestions}
                onSuggestionsFetchRequested={this.suggestionsFetchRequested}
                onSuggestionsClearRequested={this.suggestionsClearRequested}
                renderSuggestionsContainer={this.renderSuggestionsContainer}
                getSuggestionValue={s => s}
                renderSuggestion={this.renderSuggestion}
                inputProps={{
                    placeholder: 'Search...',
                    value: this.state.value,
                    onChange: this.inputChanged,
                }}/>
        )
    }
}

export default SearchList2;
