import React, { Component } from 'react';
import CustomList from './CustomList';

class CategoryList extends Component {
    render() {
        const size = 14;
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