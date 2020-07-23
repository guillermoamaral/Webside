import React, { Component } from 'react';
import {
    Grid,
    Accordion,
    AccordionSummary,
    AccordionDetails,
    Typography
} from '@material-ui/core';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import InspectorIcon from '../icons/InspectorIcon';
import CodeEditor from '../parts/CodeEditor';
import Inspector from './Inspector';
import { AppContext } from '../../AppContext';

class Workspace extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            expression: '1 @ 2 extent: 10',
            opensInspector: true,
            inspectors: [],
        };
    }

    openInspector(object) {
        const inspector = <Inspector
          key={object.id}
          classes={this.props.classes}
          root={object}
          onClose={this.closeInspector}/>;
        const inspectors = this.state.inspectors;
        inspectors.unshift(inspector);
        this.setState({inspectors: inspectors})
    }

    closeInspector = (id) => {
        this.setState({inspectors: this.state.inspectors.filter((i) => {return i.props.root.id !== id})});
    }
    
    expressionChanged = (text) => {
        this.setState({expression: text})
    }

    evaluateClicked = () => {
        this.context.api.evaluate(this.state.expression, true)
            .then(object => {
                if (this.state.opensInspector) {
                    this.openInspector(object)
                } else {
                    this.setState({expression: this.state.expression + ' -> ' + object.printString})
                }
            })
    }

    render() {
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={8} lg={8}>
                    <Grid item xs={12} md={12} lg={12}>
                        <CodeEditor
                            classes={this.props.classes}
                            source={this.state.expression}
                            showAccept={true}
                            onAccept={this.evaluateClicked}
                            onChange={this.expressionChanged}/>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={4} lg={4}>
                    {this.state.inspectors.map((inspector, index) => {
                        return (
                            <Accordion key={inspector.key} defaultExpanded>
                                <AccordionSummary
                                    expandIcon={<ExpandMoreIcon />}
                                    id="panel1a-header"
                                    >
                                    <InspectorIcon/>
                                    <Typography>
                                        {inspector.props.root.class + ': ' + inspector.props.root.id}
                                    </Typography>
                                </AccordionSummary>
                                {inspector}
                            </Accordion>
                        )         
                    })}
                </Grid>
            </Grid>
        )
    }
}

export default Workspace;
