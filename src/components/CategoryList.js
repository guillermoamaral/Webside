import React, { Component } from 'react';
import CustomList from './CustomList';

class CategoryList extends Component {
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
        return (
            <CustomList
                items={this.props.categories}
                selectedItem={this.props.selectedCategory}
                onSelect={this.props.onSelect}
                menuOptions={[
                    {label: 'Add', action: this.addCategory},
                    {label: 'Rename', action: this.renameCategory},
                    {label: 'Remove', action: this.removeCategory}]}
            />
        )
    }
};

export default CategoryList;