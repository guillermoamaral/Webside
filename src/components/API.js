import React from 'react';
import axios from 'axios';

class API {
    constructor(uri, reportError){
        this.baseUri = uri;
        this.reportError = reportError.bind();
    };

    handleError(prefix, error) {
        var reason;
        if (error.response) {
            reason = 'Response error: ' + error.response.status + ': ' + error.response.statusText;
        } else if (error.request) {
            reason = 'Request error: ' + error.request;
        } else {
            reason = 'Could not send request: ' + error.message;
        }
        this.reportError(prefix +  '\n' + reason);
    }

    async classTree(root) {
        try {
            const response = await axios.get(this.baseUri + '/classes?root=' + root + '&tree=true');
            return response.data; 
        }
        catch (error) { this.handleError('Cannot fetch class tree from ' + root, error)}
    }

    async classNames() {
        try {
            const response = await axios.get(this.baseUri + '/classes?names=true')
            return response.data; 
        }
        catch (error) { this.handleError('Cannot fetch class names', error)}
    }

    async definitionOf(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname);
            return response.data;
        }
        catch (error) { this.handleError('Cannot fetch class ' + classname, error) }
    }

    async instanceVariablesOf(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/instance-variables');
            return response.data
        }
        catch (error) { this.handleError('Cannot fecth instance variables of class ' + classname, error) }
    }    

    async variablesOf(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/variables');
            return response.data
        }
        catch (error) { this.handleError('Cannot fecth variables of class ' + classname, error) }
    }

    async categoriesOf(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/categories');
            return response.data
        }
        catch (error) { this.handleError('Cannot fecth categories of class ' + classname, error) }
    }

    async selectorsOf(classname, category) {
        try {
            var url = this.baseUri + '/classes/' + classname + '/selectors?marks=true';
            if (category !== null) { url = url + '&category=' + category}
            const response = await axios.get(url);
            return response.data;
        }
        catch (error) { this.handleError('Cannot fecth selectors of class ' + classname, error) }
    }

    async method(classname, selector) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/methods/' + selector);
            return response.data;
        }
        catch (error) { this.handleError('Cannot fetch method ' + classname + '>>#' + selector, error) }
    }

    async sendersOf(selector) {
        try {
           const response = await axios.get(this.baseUri + '/methods?sending=' + selector);
           return response.data;
       }
       catch (error) { this.handleError('Cannot fetch senders of ' + selector, error) }
    }

    async implementorsOf(selector) {
         try {
            const response = await axios.get(this.baseUri + '/methods?selector=' + selector);
            return response.data;
        }
        catch (error) { this.handleError('Cannot fetch implementors of ' + selector, error) }
    }

    async defineClass(classname, definitionString, comment) {
        const definition = {
            name: classname,
            definitionString: definition,
            comment: comment};
        try {
            const response = await axios.post(this.baseUri + '/classes', definition);
            return response.data;
        }
        catch (error) { this.handleError('Cannot define class ' + classname, error) }
    }

    async compileMethod(classname, category, source) {
        try {
            const method = {class: classname, category: category, source: source};
            const response = await axios.post(this.baseUri + '/classes/' + classname + '/methods', method);
            return response.data;
        }
        catch (error) { this.handleError('Cannot compile ' + source + ' in ' + classname, error)}
    }

    async evaluate(expression) {
        try {
            const response = await axios.post(this.baseUri + '/objects', expression);
            return response.data;
        }
        catch (error) { this.handleError('Cannot evaluate ' + expression, error) }
    }

    async objects() {
        try {
            const response = await axios.get(this.baseUri + '/objects')
            return response.data
        }
        catch (error) { this.handleError('Cannot fetch objects', error)}
    }

    async object(id) {
        try {
            const response = await axios.get(this.baseUri + '/objects/' + id)
            return response.data
        }
        catch (error) { this.handleError('Cannot fetch object with id ' + id, error)}
    }

    async variableOf(id, variable) {
        try {
            const response = await axios.get(this.baseUri + '/objects/' + id + '/' + variable);
            return response.data
        }
        catch (error) { this.handleError('Cannot fecth variable ' + variable + ' of object with id ' + id, error) }
    }

}

export default API;