import React, { PureComponent } from 'react';
import { Box } from '@material-ui/core';
import PropTypes from 'prop-types';

class TabPanel extends PureComponent {
    render() {
        const {id, children, visible, ...other} = this.props;
        return (
            <div
                role="tabpanel"
                hidden={!visible}
                id={id}
                {...other}>
                <Box className={this.props.styles.box} p={1}>
                    {children}
                </Box>
            </div>
        )
    }
}
  
TabPanel.propTypes = {
    children: PropTypes.node,
    id: PropTypes.any.isRequired,
    visible: PropTypes.any.isRequired,
}

export default TabPanel;
