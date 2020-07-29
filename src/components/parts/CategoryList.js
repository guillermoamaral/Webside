import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { AppContext } from '../../AppContext';

class CategoryList extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.all = 'All selectors'; 
    }

    categorySelected = (category) => {
        const selected = category === this.all? null : category;
        const handler = this.props.onSelect;
        if (handler) {handler(selected)}
    }

    addCategory = (category) => {
        if (this.props.onAdded) {this.props.onAdded(category)}
    }

    removeCategory = (category) => {
        this.context.api.deleteCategory(this.props.class.name, category)
            .then(response => {
                if (this.props.onRemoved) {this.props.onRemoved(category)}
            })
            .catch(error => {})
    }

    render() {
        let categories = this.props.categories;
        if (categories) {
            categories = [...categories];
            categories.unshift(this.all)};
        return (
            <CustomList
                items={categories}
                itemDivider={item => item === this.all}
                selectedItem={this.props.selectedCategory}
                onSelect={this.categorySelected}
                menuOptions={[
                    {label: 'Add', action: this.addCategory},
                    {label: 'Rename', action: this.renameCategory},
                    {label: 'Remove', action: this.removeCategory}]}
            />
        )
    }
};

export default CategoryList;