# Retrieve packages

Retrieve all packages in the system.

**URL**: `/packages`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| :----: | :-----: | --------- |
| names | boolean | true to get only package names |

## Success Responses

**Code** : `200 OK`

**Content**: `[package]` where `package` is defined as:

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

**Example:**: The following is the partial result of `GET /packages` on Pharo.

```json
[
    {
        "name": "Webside",
        "classes": [
            "WebsideAPI",
            "WebsideServer",
            "ManifestWebside"
        ],
        "methods": {
            "RBVariableNode": [
                "websideType"
            ],
            "RPackageTag": [
                "asWebsideJson"
            ],
            "Dictionary": [
                "asWebsideJson"
            ],
            "RBRefactoring": [
                "fromWebsideJson:",
                "websideType",
                "acceptsWebsideJson:",
                "classForWebsideJson:"
            ],
            "RBLiteralValueNode": [
                "asWebsideJson"
            ],
            "RBCascadeNode": [
                "asWebsideJson"
            ],
            "RBVariableRefactoring": [
                "fromWebsideJson:",
                "asWebsideJson"
            ],
            "RBPullUpInstanceVariableRefactoring": [
                "websideType"
            ],
            "Object": [
                "asWebsideJson"
            ],
            "RBMethodNode": [
                "asWebsideJson"
            ],
            "RBAddMethodChange": [
                "websideType"
            ],
            "RBReturnNode": [
                "asWebsideJson"
            ],
            "RBRemoveMethodChange": [
                "websideType"
            ],
            "RBAddClassChange": [
                "websideType"
            ],
            "RBRenameMethodRefactoring": [
                "websideType"
            ],
            "RBRefactoryProtocolChange": [
                "fromWebsideJson:",
                "asWebsideJson"
            ],
            "String": [
                "asWebsideJson"
            ],
            "RPackage": [
                "asWebsideJson"
            ],
            "RBMethodProtocolChange": [
                "websideType"
            ],
            "Context": [
                "asWebsideJson"
            ],
            "SyntaxErrorNotification": [
                "asWebsideJson"
            ],
            "RBAssignmentNode": [
                "asWebsideJson"
            ],
            "ClassDescription": [
                "asWebsideJson"
            ],
            "RBCommentChange": [
                "websideType"
            ],
            "RBRefactoryChange": [
                "fromWebsideJson:",
                "websideType",
                "acceptsWebsideJson:",
                "classForWebsideJson:"
            ],
            "RBRefactoryClassChange": [
                "fromWebsideJson:",
                "asWebsideJson"
            ],
            "CompiledMethod": [
                "asWebsideJson"
            ],
            "RBRenameVariableChange": [
                "fromWebsideJson:",
                "asWebsideJson"
            ],
            "RBMessageNode": [
                "asWebsideJson"
            ],
            "RBSequenceNode": [
                "asWebsideJson"
            ],
            "RBRefactoryDefinitionChange": [
                "asWebsideJson"
            ],
            "RBMethodRefactoring": [
                "fromWebsideJson:",
                "asWebsideJson"
            ],
            "RBLiteralArrayNode": [
                "asWebsideJson"
            ],
            "RGMethodDefinition": [
                "asWebsideJson"
            ],
            "RBRefactoryVariableChange": [
                "fromWebsideJson:",
                "asWebsideJson"
            ],
            "RBRenameClassChange": [
                "websideType"
            ],
            "RBNode": [
                "websideType"
            ],
            "Collection": [
                "asWebsideJson"
            ],
            "RBRemoveProtocolChange": [
                "websideType"
            ],
            "RBAddClassVariableChange": [
                "websideType"
            ],
            "RBRemoveClassChange": [
                "websideType"
            ],
            "RBRemoveInstanceVariableChange": [
                "websideType"
            ],
            "RBReplaceMethodRefactoring": [
                "acceptsWebsideJson:"
            ],
            "RBAddMethodRefactoring": [
                "websideType",
                "acceptsWebsideJson:"
            ],
            "RBPushDownInstanceVariableRefactoring": [
                "websideType"
            ],
            "RBAddInstanceVariableChange": [
                "websideType"
            ],
            "RBRenameInstanceVariableChange": [
                "websideType"
            ]
        },
    }
    {
        "name": "Webside-Tests",
        "classes": [
            "WebsideAPITest"
        ],
        "methods": {}
    }
]
```
