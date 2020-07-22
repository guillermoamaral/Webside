import axios from 'axios';

class API {
    constructor(uri, user, reportError){
        this.baseUri = uri;
        this.reportError = reportError.bind();
        this.author = user;
    };

    handleError(prefix, error) {
        var reason;
        if (error.response) {
            reason = 'Response error ' + error.response.status + ' - ' + error.response.statusText +
                '\r on request to ' + error.request.responseURL;
        } else if (error.request) {
            reason = 'Request error ' + error.request;
        } else {
            reason = 'Could not send request ' + error.message;
        }
        this.reportError(prefix +  '\r' + reason);
        throw(error);
    }

    // Queries...
    async getClassTree(root, depth) {
        try {
            const response = await axios.get(this.baseUri + '/classes?root=' + root + '&tree=true&depth=' + depth);     
            return response.data;

         }
        catch (error) {this.handleError('Cannot fetch class tree from ' + root, error)}
    }

    async getClassNames() {
        try {
            const response = await axios.get(this.baseUri + '/classes?names=true')
            return response.data; 
        }
        catch (error) {this.handleError('Cannot fetch class names', error)}
    }

    async getClass(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname);
            return response.data;
        }
        catch (error) {this.handleError('Cannot fetch class ' + classname, error)}
    }

    async getSubclasses(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/subclasses');
            return response.data;
        }
        catch (error) {this.handleError('Cannot fecth subclasses of class ' + classname, error)}
    }

    async getInstanceVariables(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/instance-variables');
            return response.data;
        }
        catch (error) {this.handleError('Cannot fecth instance variables of class ' + classname, error)}
    }
    
    async getVariables(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/variables');
            return response.data;
        }
        catch (error) {this.handleError('Cannot fecth variables of class ' + classname, error)}
    }

    async getCategories(classname) {
        try {
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/categories');
            return response.data;
        }
        catch (error) {this.handleError('Cannot fecth categories of class ' + classname, error)}
    }

    async getMethods(classname) {
        try {
            var url = this.baseUri + '/classes/' + classname + '/methods?marks=true';
            const response = await axios.get(url);
            return response.data;
        }
        catch (error) {this.handleError('Cannot fecth methods of class ' + classname, error)}
    }

    async getMethodsUsing(classname, variable) {
        try {
            var url = this.baseUri + '/classes/' + classname + '/methods?using=' + variable;
            const response = await axios.get(url);
            return response.data;
        }
        catch (error) {this.handleError('Cannot fecth methods of class ' + classname + ' using ' + variable, error)}
    }

    async getMethod(classname, selector) {
        try {
            const encoded = encodeURIComponent(selector);
            const response = await axios.get(this.baseUri + '/classes/' + classname + '/methods?selector=' + encoded);
            return response.data.length === 0 ? null : response.data[0];
        }
        catch (error) {this.handleError('Cannot fetch method ' + classname + '>>#' + selector, error)}
    }

    async getSenders(selector) {
        try {
           const response = await axios.get(this.baseUri + '/methods?sending=' + selector);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch senders of ' + selector, error) }
    }

    async getReferences(classname) {
        try {
           const response = await axios.get(this.baseUri + '/methods?referencing=' + classname);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch references to ' + classname, error)}
    }

    async getImplementors(selector) {
         try {
            const response = await axios.get(this.baseUri + '/methods?selector=' + selector);
            return response.data;
        }
        catch (error) {this.handleError('Cannot fetch implementors of ' + selector, error)}
    }

    // Debugger...
    async getFrames(id) {
        try {
           const response = await axios.get(this.baseUri + '/debuggers/' + id + '/frames');
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch frames of debugger ' + id, error)}
    }

    async getFrame(id, index) {
        try {
           const response = await axios.get(this.baseUri + '/debuggers/' + id + '/frames/' + index);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch frame ' + index + ' of debugger ' + id, error)}
    }

    // Changes...
    async getChanges() {
        try {
           const response = await axios.get(this.baseUri + '/changes');
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch changes ', error)}
    }

    newChange(type) {
        return {
            type: type,
            author: this.user,
        }
    }

    async postChange(change) {
        try {
            const response = await axios.post(this.baseUri + '/changes', change);
            return response.data;
        }
        catch (error) {this.handleError('Cannot apply change ' + change, error)}
    }

    //Helpers...
    async defineClass(classname, definition) {
        const change = this.newChange('ClassDefinition');
        change.class = classname;
        change.definition = definition;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot define class ' + classname, error)}
    }

    async commentClass(classname, comment) {
        const change = this.newChange('ClassCommentDefinition');
        change.class = classname;
        change.comment = comment;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot comment class ' + classname, error)}
    }

    async deleteClass(classname) {
        const change = this.newChange('ClassRemove');
        change.class = classname;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot remove class ' + classname, error)}
    }

    async deleteCategory(classname, category) {
        const change = this.newChange('CategoryRemove');
        change.class = classname;
        change.category = category;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot remove category ' + category + ' from class ' + classname, error)}
    }

    async compileMethod(classname, category, source) {
        try {
            const change = this.newChange('MethodDefinition');
            change.class = classname;
            change.category = category;
            change.sourceCode = source;
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot compile ' + source + ' in ' + classname, error)}
    }

    async deleteMethod(classname, selector) {
        const change = this.newChange('MethodRemove');
        change.class = classname;
        change.selector = selector;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot remove methodd ' + classname + '>>#' + selector, error)}
    }

    // Objects...
    async evaluate(expression, pins) {
        try {
            const value = pins === undefined ? false : pins;
            const response = await axios.post(this.baseUri + '/objects?pins=' + value, expression);
            return response.data;
        }
        catch (error) {this.handleError('Cannot evaluate ' + expression, error)}
    }

    async getObjects() {
        try {
            const response = await axios.get(this.baseUri + '/objects')
            return response.data
        }
        catch (error) {this.handleError('Cannot fetch objects', error)}
    }

    async getObject(id) {
        try {
            const response = await axios.get(this.baseUri + '/objects/' + id)
            return response.data
        }
        catch (error) {this.handleError('Cannot fetch object with id ' + id, error)}
    }

    async deleteObject(id) {
        try {
            const response = await axios.delete(this.baseUri + '/objects/' + id)
            return response.data
        }
        catch (error) {this.handleError('Cannot fetch object with id ' + id, error)}
    }

    async getVariable(id, path) {
        try {
            const response = await axios.get(this.baseUri + '/objects/' + id + path);
            return response.data
        }
        catch (error) {this.handleError('Cannot fecth variable ' + path + ' of object with id ' + id, error)}
    }

}

export default API;