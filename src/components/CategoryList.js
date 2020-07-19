import React, { Component } from 'react';
import CustomList from './CustomList';

class CategoryList extends Component {
    constructor(props) {
        super(props);
        this.all = 'All selectors'; 
    }

    categorySelected = (category) => {
        const selected = category === this.all? null : category;
        const handler = this.props.onSelect;
        if (handler !== undefined) {handler(selected)}
    }

    addCategory = (category) => {
        if (this.props.onAdded !== undefined) {
            this.props.onAdded(category)
        }
    }

    removeCategory = (category) => {
        this.props.api.deleteCategory(this.props.class.name, category)
            .then(response => {
                if (this.props.onRemoved !== undefined) {
                    this.props.onRemoved(category)
                }
            })
            .catch(error => {})
    }

    render() {
        let categories = this.props.categories;
        if (categories !== undefined) {
            categories = [...categories];
            categories.unshift(this.all)};
        return (
            <CustomList
                items={categories}
                itemDivider={item => {return item === this.all}}
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