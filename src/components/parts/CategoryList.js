import React, { Component } from 'react';
import CustomList from '../controls/CustomList';
import { AppContext } from '../../AppContext';
import { withDialog } from '../dialogs';

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

    addCategory = () => {
        this.props.dialog.prompt('New category')
            .then(category => {
                if (category && this.props.onAdd) {this.props.onAdd(category)}
            })
            .catch(() => {})
    }

    renameCategory = async (category) => {
        if (!category) {return}
        try {
            const newName = this.props.dialog.prompt({title: 'Rename category', defaultValue: category});
            await this.context.api.renameCategory(this.props.class.name, category, newName);
            if (this.props.onRename) {this.props.onRename(category, newName)}
        }
        catch (error) {}
    }

    removeCategory = async (category) => {
        await this.context.api.deleteCategory(this.props.class.name, category);
        if (this.props.onRemove) {this.props.onRemove(category)}
    }

    menuOptions() {
        return [
            {label: 'Add', action: this.addCategory},
            {label: 'Rename', action: this.renameCategory},
            {label: 'Remove', action: this.removeCategory}
        ]
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
                menuOptions={this.menuOptions()}
            />
        )
    }
};

export default withDialog()(CategoryList);