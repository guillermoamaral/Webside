import React, { Component } from 'react';
import {
    Grid,
    Accordion,
    AccordionSummary,
    AccordionDetails,
    Typography,
    IconButton
} from '@material-ui/core';
import CloseIcon from '@material-ui/icons/Close';
import PlayIcon from '@material-ui/icons/PlayArrow';
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
            evaluating: false,
        };
    }

    openInspector(object) {
        const inspector = <Inspector
          key={object.id}
          styles={this.props.styles}
          root={object}/>;
        const inspectors = this.state.inspectors;
        inspectors.unshift(inspector);
        this.setState({inspectors: inspectors})
    }

    closeInspector = async (event, id) => {
        event.stopPropagation();
        await this.context.api.unpinObject(id);
        this.setState({inspectors: this.state.inspectors.filter(i => i.key !== id)});
    }
    
    expressionChanged = (text) => {
        this.setState({expression: text})
    }

    evaluateClicked = async () => {
        try {
            this.setState({evaluating: true});
            const object = await this.context.evaluateExpression(this.state.expression, true);
            if (this.state.opensInspector) {
                this.setState({evaluating: false});
                this.openInspector(object);
            } else {
                this.setState({
                    expression: this.state.expression + ' -> ' + object.printString,
                    evaluating: false})
            }
        }
        catch (error) {this.setState({evaluating: false})}
    }

    render() {
        return (
            <Grid container spacing={1}>
                <Grid item xs={12} md={8} lg={8}>
                    <Grid item xs={12} md={12} lg={12}>
                        <CodeEditor
                            styles={this.props.styles}
                            lineNumbers={true}
                            source={this.state.expression}
                            showAccept={true}
                            acceptIcon={<PlayIcon/>}
                            onAccept={this.evaluateClicked}
                            onChange={this.expressionChanged}
                            evaluating={this.state.evaluating}/>
                    </Grid>
                </Grid>
                <Grid item xs={12} md={4} lg={4}>
                    {this.state.inspectors.map((inspector, index) => {
                        return (
                            <Accordion key={inspector.key} defaultExpanded>
                                <AccordionSummary
                                    expandIcon={<ExpandMoreIcon/>}
                                    id="panel1a-header">
                                        <IconButton 
                                            onClick={event => {this.closeInspector(event, inspector.key)}}
                                            size="small">
                                                <CloseIcon fontSize="small"/>
                                        </IconButton>
                                        <InspectorIcon/>
                                        <Typography>
                                            {'Inspecting: ' + inspector.props.root.class}
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
