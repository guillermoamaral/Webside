import axios from 'axios';

class API {
    constructor(uri, user, reportError, reportChange){
        this.baseUri = uri;
        this.reportError = reportError.bind();
        this.reportChange = reportChange.bind();
        this.author = user;
    };

    handleError(prefix, error, report = true) {
        var reason;
        var data;
        if (error.response) {
            reason = 'Response error ' + error.response.status + ' - ' + error.response.statusText +
                '\r on request to ' + error.request.responseURL;
            data = error.response.data;
        } else if (error.request) {
            reason = 'Request error ' + error.request;
            data = error.request;
        } else {
            reason = 'Could not send request ' + error.message;
            data = error;
        }
        if (report) {this.reportError(prefix +  '\r' + reason)}
        throw(data);
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

    async getMethodsReferencing(classname, variable) {
        try {
            var url = this.baseUri + '/classes/' + classname + '/methods?referencingVariable=' + variable;
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

    async getLocalSenders(selector, classname) {
        try {
           const response = await axios.get(this.baseUri + '/methods?sending=' + selector + '&scope=' + classname);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch senders of ' + selector + ' in class ' + classname, error) }
    }

    async getReferences(classname) {
        try {
           const response = await axios.get(this.baseUri + '/methods?referencingClass=' + classname);
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

    async getLocalImplementors(selector, classname) {
        try {
           const response = await axios.get(this.baseUri + '/methods?selector=' + selector + '&scope=' + classname);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch local implementors of ' + selector + ' in class ' + classname, error)}
   }

    // Debugger...
    async createDebugger(id) {
        try {
            const process = {process: id};
            const response = await axios.post(this.baseUri + '/debuggers', process);
            return response.data;
       }
       catch (error) {this.handleError('Cannot delete debugger ' + id, error)}
    }

    async getDebuggerFrames(id) {
        try {
           const response = await axios.get(this.baseUri + '/debuggers/' + id + '/frames');
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch frames of debugger ' + id, error)}
    }

    async getDebuggerFrame(id, index) {
        try {
           const response = await axios.get(this.baseUri + '/debuggers/' + id + '/frames/' + index);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch frame ' + index + ' in debugger ' + id, error)}
    }

    async getFrameBindings(id, index) {
        try {
           const response = await axios.get(this.baseUri + '/debuggers/' + id + '/frames/' + index + '/bindings');
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch bindings of frame ' + index + ' in debugger ' + id, error)}
    }

    async hopDebugger(id, index) {
        try {
           const response = await axios.post(this.baseUri + '/debuggers/' + id + '/hop?frame=' + index);
           return response.data;
       }
       catch (error) {this.handleError('Cannot hop on frame '+ index + ' of debugger ' + id, error)}
    }

    async skipDebugger(id, index) {
        try {
           const response = await axios.post(this.baseUri + '/debuggers/' + id + '/skip?frame=' + index);
           return response.data;
       }
       catch (error) {this.handleError('Cannot skip on frame '+ index + ' of debugger ' + id, error)}
    }

    async restartDebugger(id, index) {
        try {
           const response = await axios.post(this.baseUri + '/debuggers/' + id + '/restart?frame=' + index);
           return response.data;
       }
       catch (error) {this.handleError('Cannot skip on frame '+ index + ' of debugger ' + id, error)}
    }

    async resumeDebugger(id) {
        try {
           const response = await axios.post(this.baseUri + '/debuggers/' + id + '/resume');
           return response.data;
       }
       catch (error) {this.handleError('Cannot resume debugger ' + id, error)}
    }

    async terminateDebugger(id) {
        try {
           const response = await axios.post(this.baseUri + '/debuggers/' + id + '/terminate');
           return response.data;
       }
       catch (error) {this.handleError('Cannot terminate debugger ' + id, error)}
    }

    async deleteDebugger(id) {
        try {
           const response = await axios.delete(this.baseUri + '/debuggers/' + id);
           return response.data;
       }
       catch (error) {this.handleError('Cannot delete debugger ' + id, error)}
    }

    // Changes...
    async getChanges() {
        try {
           const response = await axios.get(this.baseUri + '/changes?author=' + this.author);
           return response.data;
       }
       catch (error) {this.handleError('Cannot fetch changes ', error)}
    }

    newChange(type) {
        return {
            type: type,
            author: this.author,
        }
    }

    async postChange(change) {
        const response = await axios.post(this.baseUri + '/changes', change);
        this.reportChange(change);
        return response.data;
    }

    // Change helpers...
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
    async renameCategory(classname, category, newName) {
        const change = this.newChange('CategoryRename');
        change.class = classname;
        change.category = category;
        change.newName = newName;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot rename category ' + category + ' to ' + newName, error)}
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

    async renameSelector(classname, selector, newSelector) {
        const change = this.newChange('SelectorRename');
        change.class = classname;
        change.selector = selector;
        change.newSelector = newSelector;
        try {
            return await this.postChange(change);
        }
        catch (error) {this.handleError('Cannot rename selector ' + selector + ' to ' + newSelector, error)}
    }

    // Evaluations...
    async evaluateExpression(expression, synch = false, pin = false) {
        try {
            const evaluation = {
                expression: expression,
                context: null
            }
            const response = await axios.post(this.baseUri + '/evaluations?synch=' + synch + '&pin=' + pin, evaluation);
            return response.data;
        }
        catch (error) {this.handleError('Cannot evaluate ' + expression, error, false)}
    }

    async debugExpression(expression) {
        try {
            const response = await axios.post(this.baseUri + '/debuggers', expression);
            return response.data;
        }
        catch (error) {this.handleError('Cannot debug ' + expression, error)}
    }

    // Objects...
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
        catch (error) {
            const report = !error.response || !error.response.data || !error.response.data.process;
            this.handleError('Cannot fetch object with id ' + id, error, report)}
    }

    async unpinObject(id) {
        try {
            const response = await axios.delete(this.baseUri + '/objects/' + id)
            return response.data
        }
        catch (error) {this.handleError('Cannot unpin object with id ' + id, error)}
    }

    async getSlot(id, path) {
        try {
            const response = await axios.get(this.baseUri + '/objects/' + id + path);
            return response.data
        }
        catch (error) {this.handleError('Cannot fecth slot ' + path + ' of object with id ' + id, error)}
    }

}

export default API;