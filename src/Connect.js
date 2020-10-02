import React, { Component } from 'react';
import {
    CssBaseline,
    Paper,
    Box,
    Button,
    TextField,
    Grid,
    Typography,
    Select,
    MenuItem
    } from "@material-ui/core";
import { withCookies } from 'react-cookie';
import { withRouter } from "react-router-dom"

const dialects = ['Bee', 'Pharo'];

class Connect extends Component {
    constructor(props) {
        super(props);
        this.state = {
            smalltalk: "Bee",
            baseUri: "",
            developer: ""
        }
    }

    handleChange = (event) => {
        this.setState({[event.target.name]: event.target.value});
        console.log(this.state)
    }

    handleSubmit = (event) => {
        event.preventDefault();
        const {smalltalk, baseUri, developer} = this.state;
        if (baseUri && baseUri !== "" && developer && developer !== "") {
            const cookies = this.props.cookies;
            cookies.set('smalltalk', smalltalk, { path: '/' });
            cookies.set('baseUri', baseUri, { path: '/' });
            cookies.set('developer', developer, { path: '/' });
            this.props.history.push("/ide");
        } else {
            alert('You should fill the fields');
        }
    }

    render() {
        const {smalltalk, baseUri, developer} = this.state;
        return (
            <div className={this.props.styles.root}>
                <CssBaseline/>
                <Grid container direction="column" justify="center" spacing={1} style={{minHeight: '80vh'}}>
                    <Grid item>
                        <Grid container direction="row" justify="center" spacing={1}>
                            <Grid item>
                                <form onSubmit={this.handleSubmit}>
                                    <Grid container direction="column" spacing={1} alignItems="flex-end">
                                        <Grid item>
                                            <Select
                                                id="smalltalk"
                                                name="smalltalk"
                                                value={smalltalk}
                                                variant="outlined"
                                                fullWidth
                                                margin="dense"
                                                onChange={this.handleChange}>
                                                    {dialects.map(s => {return <MenuItem key={s} value={s}>{s}</MenuItem>})}
                                            </Select>
                                        </Grid>
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
                                                onChange={this.handleChange}
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
                    </Grid>
                </Grid>
            </div>
        )
    }
}

export default withRouter(withCookies(Connect))