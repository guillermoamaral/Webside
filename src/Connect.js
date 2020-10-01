import React, { Component } from 'react';
import {
    CssBaseline,
    Paper,
    Box,
    Button,
    TextField,
    Grid,
    Typography
    } from "@material-ui/core";
import { withCookies } from 'react-cookie';
import { withRouter } from "react-router-dom"

class Connect extends Component {
    constructor(props) {
        super(props);
        this.state = {
            baseUri: "",
            developer: ""
        }
    }

    handleChange = (event) => {
        this.setState({[event.target.name]: event.target.value});
    }

    handleSubmit = (event) => {
        event.preventDefault();
        const {baseUri, developer} = this.state;
        if (baseUri && baseUri !== "" && developer && developer !== "") {
            const cookies = this.props.cookies;
            cookies.set('baseUri', baseUri, { path: '/' });
            cookies.set('developer', developer, { path: '/' });
            this.props.history.push("/ide");
        } else {
            alert('You should fill the fields');
        }
    }

    render() {
        return (
            <div className={this.props.styles.root}>
                <CssBaseline/>
                <Grid container direction="column" justify="center" spacing={2} style={{minHeight: '100vh'}}>
                    <Grid item>
                        <Paper variant="outlined">
                        <Grid container direction="row" justify="center" spacing={2}>
                            <Grid item>
                                <form onSubmit={this.handleSubmit}>
                                    <Grid container direction="column" spacing={1} alignItems="flex-end">
                                        <Grid item>
                                            <TextField
                                                label="Host Smalltalk (URL)"
                                                type="url"
                                                placeholder="URL"
                                                margin="dense"
                                                fullWidth
                                                name="baseUri"
                                                variant="outlined"
                                                value={this.state.baseUri}
                                                onChange={this.handleChange}
                                                required
                                                autoFocus/>
                                        </Grid>
                                        <Grid item>
                                            <TextField
                                                label="Nickname"
                                                type="text"
                                                placeholder="developer"
                                                margin="dense"
                                                fullWidth
                                                name="developer"
                                                variant="outlined"
                                                value={this.state.developer}
                                                onChange={this.handleChange}
                                                required/>
                                        </Grid>
                                        <Grid item>
                                            <Button variant="outlined" color="primary" type="submit">
                                                Connect
                                            </Button>
                                        </Grid>
                                    </Grid>
                                </form>
                            </Grid>
                        </Grid>
                        </Paper>
                    </Grid>
                </Grid>
            </div>
        )
    }
}

export default withRouter(withCookies(Connect))