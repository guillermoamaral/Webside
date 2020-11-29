import React, { Component } from 'react';
import {
    Button,
    TextField,
    Grid
    } from "@material-ui/core";
import { withCookies } from 'react-cookie';
import { withRouter } from "react-router-dom";
import axios from 'axios';

class Connect extends Component {
    constructor(props) {
        super(props);
        const cookies = this.props.cookies;
        const baseUri = cookies.get('baseUri') || "";
        const developer = cookies.get('developer') || "";
        this.state = {
            baseUri: baseUri,
            developer: developer,
        }
    }

    connectClicked = async (event) => {
        event.preventDefault();
        const {baseUri, developer} = this.state;
        if (baseUri && baseUri !== "" && developer && developer !== "") {
            const cookies = this.props.cookies;
            var dialect;
            try {
                const response = await axios.get(baseUri + '/dialect');
                dialect = response.data;
            }
            catch(error) {console.log(error)}
            cookies.set('dialect', dialect, { path: '/' });
            cookies.set('baseUri', baseUri, { path: '/' });
            cookies.set('developer', developer, { path: '/' });
            this.props.history.push("/ide");
        } else {
            alert('You must complete the fields');
        }
    }

    getDialect = async () => {
        try {
          this.dialect = await this.api.getDialect();
          this.theme = this.createTheme();
        }
        catch (error) {this.reportError(error)}
    }

    render() {
        const {baseUri, developer} = this.state;
        return (
            <div className={this.props.styles.root}>
                <Grid container direction="column" justify="center" spacing={1} style={{minHeight: '80vh'}}>
                    <Grid item>
                        <Grid container direction="row" justify="center" spacing={1}>
                            <Grid item>
                                <form onSubmit={this.connectClicked}>
                                    <Grid container direction="column" spacing={1} alignItems="flex-end">
                                        <Grid item>
                                            <TextField
                                                id="baseUri"
                                                label="Host Smalltalk (URL)"
                                                type="url"
                                                placeholder="URL"
                                                margin="dense"
                                                fullWidth
                                                name="baseUri"
                                                variant="outlined"
                                                value={baseUri}
                                                onChange={event => this.setState({baseUri: event.target.value})}
                                                required
                                                autoFocus/>
                                        </Grid>
                                        <Grid item>
                                            <TextField
                                                id="developer"
                                                label="Nickname"
                                                type="text"
                                                placeholder="developer"
                                                margin="dense"
                                                fullWidth
                                                name="developer"
                                                variant="outlined"
                                                value={developer}
                                                onChange={event => this.setState({developer: event.target.value})}
                                                required/>
                                        </Grid>
                                        <Grid item>
                                            <Button variant="outlined" type="submit">
                                                Connect
                                            </Button>
                                        </Grid>
                                    </Grid>
                                </form>
                            </Grid>
                        </Grid>
                    </Grid>
                </Grid>
            </div>
        )
    }
}

export default withRouter(withCookies(Connect))