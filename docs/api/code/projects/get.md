# Retrieve projects
Retrieve all subprojects of a given root project (if no root is provided, the uppermost project in the system is used).
It is also possible get a tree-like structure as well as to limit the depth in the projects hierarchy.    

**URL**: `/projects`

**Method**: `GET`

**Query Options**
| Parameter | Type | Description |
| ------------- | ------------- | ------------- |
| names | boolean | true to get only project names | 
| tree | boolean | true to get a tree-like structure |
| depth | number | to limit the hierarchy depth |

## Success Responses

**Code** : `200 OK`

**Content**: `[projet]` where `project` is defined as
```json
{
    "name": "string",
    "classNames": "[string]",
    "methodSignatures": "[method signature]"
}
```

_Note: properties common to every `object` are also included_

**Example: _Webside projects_**: `GET /projects?root=Webside`.
```json
[
    {
        "name": "Webside Base",
        "classNames": [
            "WebsideWorkspace",
            "WebsideCompilationError",
            "WebsideEvaluation",
            "WebsideObject",
            "WebsideEvaluationError",
            "WebsideCompilationErrorHandler",
            "WebsideAPI",
            "WebsideDebugger",
            "WebsideAbstractObject"
        ],
        "methodSignatures": []
    },
    {
        "name": "Webside Extensions",
        "classNames": [],
        "methodSignatures": [
            "ExpressionEvaluation -> asWebsideJson",
            "Species -> asWebsideJson",
            "CategoryRename -> asWebsideJson",
            "MethodChange -> asWebsideJson",
            "BeeProject -> asWebsideJson",
            "ChangeRecord class -> acceptsWebsideJson:",
            "SelectorRename -> fromWebsideJson:",
            "ClassCommentDefinition -> asWebsideJson",
            "MethodChange -> fromWebsideJson:",
            "TestResult -> asWebsideJsonSummary",
            "VariableChange -> fromWebsideJson:",
            "ChangeRecord class -> fromWebsideJson:",
            "ClassVariableMoveDown -> asWebsideJson",
            "ClassDefinition -> fromWebsideJson:",
            "ClassChange -> asWebsideJson",
            "InstanceVariableMoveDown -> fromWebsideJson:",
            "InstanceVariableRename -> fromWebsideJson:",
            "ClassRename -> asWebsideJson",
            "ProfiledMethod -> asWebsideJson",
            "Species -> localImplementorsOf:",
            "InstanceVariableRename -> asWebsideJson",
            "BeeModule -> asWebsideJson",
            "ClassChange -> fromWebsideJson:",
            "MethodDefinition -> fromWebsideJson:",
            "ChangeRecord -> fromWebsideJson:",
            "ClassVariableMoveDown -> fromWebsideJson:",
            "MethodDefinition -> asWebsideJson",
            "ClassRename -> fromWebsideJson:",
            "ProcessFrameDecorator -> asWebsideJson",
            "CompiledMethod -> asWebsideJson",
            "ChangeRecord -> asWebsideJson",
            "ExpressionEvaluation -> fromWebsideJson:",
            "CategoryRename -> fromWebsideJson:",
            "SelectorRename -> asWebsideJson",
            "VariableChange -> asWebsideJson",
            "ClassCommentDefinition -> fromWebsideJson:",
            "InstanceVariableMoveDown -> asWebsideJson",
            "CategoryChange -> asWebsideJson",
            "TestCase -> asWebsideJson",
            "ChangeRecord class -> classForWebsideJson:",
            "TestResult -> asWebsideJson",
            "CategoryChange -> fromWebsideJson:",
            "TestSuiteRunner -> asWebsideJson",
            "Object -> asWebsideJson",
            "ClassDefinition -> asWebsideJson"
        ]
    }
]
```