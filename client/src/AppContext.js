import React from 'react';

const context = {
    api: null,
    handleError: null,
    browseSenders: null,
    browseImplementors: null,
    browseReferences: null,
    openInspector: null};

export const AppContext = React.createContext(context);