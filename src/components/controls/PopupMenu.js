import React, { Component } from 'react';
import { Menu, MenuItem, Divider } from '@material-ui/core';

class PopupMenu extends Component {
    createItems = (options) => {
        if (!options) {return []};
        return (
            options.map((option, index) => {
                 if (!option) {
                    return <Divider key={"divider-" + index}/>
                } else {
                    return (
                        <MenuItem
                            key={option.label}
                            id={option.id}
                            onClick={option.suboptions? null : event => this.itemClicked(event, option)}
                            children= {this.createItems(option.suboptions)}
                            style={{paddingTop: 0, paddingBottom: 0}}>
                                {option.label}
                        </MenuItem>)
                }
            })
        )
    }

    position() {
        if (this.props.position && this.props.position.x && this.props.position.y) {
            return {left: this.props.position.x, top: this.props.position.y};
        }
    }
    
    itemClicked = (event, option) => {
        event.stopPropagation();
        this.close();
        option.action();

    }
    
    close = () => {
        const handler = this.props.onClose;
        if (handler) {
            handler.bind(this);
            handler()
        }
    }

    render() {
        return (
            <Menu
                keepMounted
                open={this.props.open}
                onClose={this.close}
                anchorReference="anchorPosition"
                anchorPosition={this.position()}>
                    {this.createItems(this.props.options)}
            </Menu>
        )
    }
}

export default PopupMenu;