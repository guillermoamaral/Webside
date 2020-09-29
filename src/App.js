import React, { Component } from 'react';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  } from "react-router-dom";
import Connect from './Connect';
import IDE from './IDE';

class App extends Component {  
  render() { 
    return (
      <Router>
          <Switch>
            <Route path="/" exact component={Connect}/>
            <Route path="/ide/" exact component={IDE}/>
          </Switch>
      </Router>
    )
  }
}

export default App;
