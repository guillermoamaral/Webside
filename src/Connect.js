import React, { Component } from 'react';
import { CssBaseline } from '@material-ui/core';
import { withCookies } from 'react-cookie';

class Connect extends Component {
    render() {
        const baseUri = 'http://localhost:9000/beeva';
        const cookies = this.props.cookies;
        cookies.set('baseUri', baseUri, { path: '/' });

        return (
            <div className={this.props.styles.root}>
                <CssBaseline/>
                Connect to some image...
            </div>
        )
    }
}

export default withCookies(Connect);