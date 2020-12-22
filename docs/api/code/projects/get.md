# Retrieve projects
Retrieve all subprojects of a given root project (if no root is provided, the uppermost project in the system is used).
It is also possible get a tree-like structure as well as to limit the depth in the projects hierarchy.    

**URL**: `/projects`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| ------------- | ------------- | ------------- |
| root | string | the name of the root project |
| names | boolean | true to get only project names |
| tree | boolean | true to get a tree-like structure |
| depth | number | to limit the hierarchy depth (only when tree=true) |

## Success Responses

**Code** : `200 OK`

**Content**: `[project]` where `project` is defined as:
```json
{
    "name": "string",
    "classes": "[string]",
    "methods": [{"class": "string", "selector": "string"}]
}
```

**Example:**: `Method Wrappers` project `GET /projects/Method Wrappers`.
```json
{
    "name": "Method Wrappers",
    "classes": [
        "FullMethodWrapper",
        "MessageSpy",
        "MessageHalt",
        "MessageInterceptor",
        "ExecutionStop",
        "MethodWrapperTest",
        "MethodWrapperTestResource",
        "MessageDeflector",
        "MethodWrapper"
    ],
    "methods": [
        {
            "class": "CompiledMethod",
            "selector": "afterInvokingEvaluate:"
        },
        {
            "class": "CompiledMethod",
            "selector": "whenInvokingEvaluate:"
        },
        {
            "class": "CompiledMethod",
            "selector": "whenInvokedEvaluate:afterThat:"
        },
        {
            "class": "Object",
            "selector": "whenReceiving:evaluate:afterThat:"
        },
        {
            "class": "Object",
            "selector": "stopSpyingMessages"
        },
        {
            "class": "BlockClosure",
            "selector": "stopWhen:receives:"
        },
        {
            "class": "CompiledMethod",
            "selector": "uninstallWrapper"
        },
        {
            "class": "Object",
            "selector": "answerTo:with:"
        },
        {
            "class": "Message",
            "selector": "ignore"
        },
        {
            "class": "Object",
            "selector": "countMessagesWhile:"
        },
        {
            "class": "Object",
            "selector": "removeHaltOn:"
        },
        {
            "class": "CompiledMethod",
            "selector": "clear"
        },
        {
            "class": "Object",
            "selector": "spyMessagesUsing:"
        },
        {
            "class": "CompiledMethod",
            "selector": "wasCalled"
        },
        {
            "class": "CompiledMethod",
            "selector": "wrapper"
        },
        {
            "class": "Message",
            "selector": "answer:"
        },
        {
            "class": "Object",
            "selector": "aboutToSend:"
        },
        {
            "class": "MessageSend",
            "selector": "uninstallSpy"
        },
        {
            "class": "Object",
            "selector": "answerTo:evaluating:while:"
        },
        {
            "class": "Object",
            "selector": "aboutToSend:count:"
        },
        {
            "class": "Object",
            "selector": "afterReceiving:evaluate:"
        },
        {
            "class": "Object",
            "selector": "send:thru:"
        },
        {
            "class": "Object",
            "selector": "stopSpying:"
        },
        {
            "class": "Object",
            "selector": "answerTo:evaluating:"
        },
        {
            "class": "Object",
            "selector": "uninstallSpyOn:"
        },
        {
            "class": "CompiledMethod",
            "selector": "getWrapper"
        },
        {
            "class": "Object",
            "selector": "whenReceiving:evaluate:"
        },
        {
            "class": "Closure",
            "selector": "stopWhen:receives:"
        },
        {
            "class": "Object",
            "selector": "haltOn:"
        },
        {
            "class": "Object",
            "selector": "ignoreNext:"
        },
        {
            "class": "Object",
            "selector": "justSent:"
        }
    ]
}
```