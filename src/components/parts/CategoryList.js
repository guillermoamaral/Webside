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
                if (category && this.props.onAdded) {this.props.onAdded(category)}
            })
            .catch(() => {})
    }

    renameCategory = (category) => {
        if (!category) {return}
        this.props.dialog.prompt({title: 'Rename category', defaultValue: category})
            .then(async renamed => {
                await this.context.api.renameCategory(this.props.class.name, category, renamed);
                if (renamed && this.props.onRenamed) {this.props.onRenamed(category, renamed)}
            })
            .catch(() => {})
    }

    removeCategory = async (category) => {
        await this.context.api.deleteCategory(this.props.class.name, category);
        if (this.props.onRemoved) {this.props.onRemoved(category)}
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

export default withDialog()(CategoryList);