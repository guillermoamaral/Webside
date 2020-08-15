import React, { Component } from 'react';
import { Menu, MenuItem, Divider } from '@material-ui/core';

class PopupMenu extends Component {
    createItems = () => {
        if (!this.props.options) {return []};
        return (
            this.props.options.map((option, index) => {
                 if (!option) {
                    return <Divider key={"divider-" + index}/>
                } else {
                    return (
                        <MenuItem
                            key={option.label}
                            id={option.id}
                            onClick={event => this.itemClicked(event, option)}
                            style={{paddingTop: 0, paddingBottom: 0}}
                        >
                            {option.label}
                        </MenuItem>
                    )
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
                {this.createItems()}
            </Menu>
        )
    }
}

export default PopupMenu;