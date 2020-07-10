import React, { Component } from 'react'

class PopupMenu extends Component {
    constructor(props){
        super(props);
        this.state = {
            open: this.props.open,
            position: {
                x: props.position === undefined ? null : props.position.x,
                y: props.position === undefined ? null : props.position.y,
            }
        }
        
    }

    createItems = () => {
        if (this.props.options === undefined) { return [] };
        return (
            this.props.options.map(option => {
                return (
                    <MenuItem
                        key={option.label}
                        onClick={this.itemClicked}
                        style={{paddingTop: 0, paddingBottom: 0}}
                    >
                        {option.label}
                    </MenuItem>
                )
            })
        )
    }

    position() {
        if (this.state.position.y !== null || this.state.menu.x !== null) {
            return undefined;
        }
        return {left: this.state.position.x, top: this.state.position.y}
    }
    
    itemClicked = (event) => {
        console.log(event.target);
        this.close()
      }
    
    close() {
        this.setState({open: false})
    }

    render() {
        return (
            <Menu
                keepMounted
                open={this.state.open}
                onClose={this.closeMenu}
                anchorReference="anchorPosition"
                anchorPosition={this.position()}
                >
                {this.createMenuItems()}
            </Menu>
        )
    }
}

export default PopupMenu;