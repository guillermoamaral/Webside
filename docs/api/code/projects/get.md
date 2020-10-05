# Retrieve classes

Retrieve all subprojects of a given root project (the uppermost project in the system if no root is provided).
It is also possible get a tree structure by using `tree=true` option as well as to limit the depth in the hierarchy by using `depth=[n]` option.    

**URL** : `/projects`

**Method** : `GET`

**Auth required** : No

**Permissions required** : None

**Data constraints** : `{}`

## Success Responses

**Code** : `200 OK`

**Content** : `{[]}`

**Example 1: Webside projects** : `GET /projects?root=Webside`.

```json
[
    {
        "class": "BeeProject",
        "indexable": false,
        "size": 0,
        "printString": "Webside Base",
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
        "class": "BeeProject",
        "indexable": false,
        "size": 0,
        "printString": "Webside Extensions",
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