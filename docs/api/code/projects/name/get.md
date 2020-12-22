# Retrieve a project
Retrieve a given project.    

**URL**: `/projects/{name}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `project` defined as:
```json
{
    "name": "string",
    "classes": ["string"],
    "methods": [
        {
            "class": "string",
            "selector": "string"
        }
    ]
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