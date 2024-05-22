# Retrieve package classes

Retrieve classes of a given package.

**URL**: `/packages/{name}/classes`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[class]` as defined in (../../../classes/get.md):

**Example:**: classes of `Webside` package `GET /packages/Webside/classes`.

```json
[
    {
        "name": "WebsideServer",
        "definition": "Object subclass: #WebsideServer\r\tinstanceVariableNames: 'server apiClass baseUri port resources'\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Webside-Base'",
        "superclass": "Object",
        "comment": "Handy class to make WebsideAPI accessible on a Teapot server.\r\rWebsideServer allInstances.\r\rWebsideServer new\r\tbaseUri: '/pharo';\r\tport: 9001;\r\tstart",
        "category": "Base",
        "variable": false,
        "package": "Webside"
    },
    {
        "name": "WebsideAPI",
        "definition": "Object subclass: #WebsideAPI\r\tinstanceVariableNames: 'request server'\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Webside-Base'",
        "superclass": "Object",
        "comment": "self startServer\r\rself stopServer\r",
        "category": "Base",
        "variable": false,
        "package": "Webside"
    },
]
```
