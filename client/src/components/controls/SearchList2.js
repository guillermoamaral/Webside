import React from 'react';
import Autosuggest from 'react-autosuggest';
import match from 'autosuggest-highlight/match';
import parse from 'autosuggest-highlight/parse';
import { TextField, Paper, MenuItem, Box } from '@material-ui/core';
import { withStyles } from '@material-ui/core/styles';

const styles = theme => ({
    container: {
      flexGrow: 1,
      position: 'relative',
      //height: 250,
    },
    suggestion: {
      display: 'block',
    },
    suggestionsList: {
      margin: 0,
      padding: 0,
      listStyleType: 'none',
    },
})

const suggestionLimit = 5;

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
                const keep = count < suggestionLimit && o.toLowerCase().slice(0, inputLength) === inputValue;
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
            <Box {...containerProps} zIndex="tooltip">
                {children}
            </Box>
        )
    }

    renderSuggestion = (suggestion, {query, isHighlighted}) => {
        const matches = match(suggestion, query);
        const parts = parse(suggestion, matches);
        return (
            <MenuItem
                selected={isHighlighted}
                component="div"
                onClick={event => this.valueChanged(suggestion)}
                style={{listStyleType: 'none'}}>
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
        const { classes } = this.props;
        return (
            <Autosuggest
                theme={{
                    container: classes.container,
                    suggestionsList: classes.suggestionsList,
                    //suggestion: classes.suggestion,
                }}
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
                    onChange: this.inputChanged}}/>
        )
    }
}

export default withStyles(styles)(SearchList2);
