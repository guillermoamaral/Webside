# Retrieve a package

Retrieve a given package.

**URL**: `/packages/{name}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `package` defined as:

```json
{
	"name": "string",
	"classes": ["string"],
	"methods": {
		"[classname]": ["string"]
	},
	"categories": ["category"]
}
```

Where `category` is defined as:

```json
{
	"name": "string",
	"package": "string"
}
```

**Example:**: `Webside` package `GET /packages/Webside`.

```json
{
	"name": "Webside",
	"classes": ["WebsideAPI", "WebsideServer", "ManifestWebside"],
	"methods": {
		"RBVariableNode": ["websideType"],
		"RPackageTag": ["asWebsideJson"],
		"Dictionary": ["asWebsideJson"],
		"RBRefactoring": [
			"fromWebsideJson:",
			"websideType",
			"acceptsWebsideJson:",
			"classForWebsideJson:"
		],
		"RBLiteralValueNode": ["asWebsideJson"],
		"RBCascadeNode": ["asWebsideJson"],
		"RBVariableRefactoring": ["fromWebsideJson:", "asWebsideJson"],
		"RBPullUpInstanceVariableRefactoring": ["websideType"],
		"Object": ["asWebsideJson"],
		"RBMethodNode": ["asWebsideJson"],
		"RBAddMethodChange": ["websideType"],
		"RBReturnNode": ["asWebsideJson"],
		"RBRemoveMethodChange": ["websideType"],
		"RBAddClassChange": ["websideType"],
		"RBRenameMethodRefactoring": ["websideType"],
		"RBRefactoryProtocolChange": ["fromWebsideJson:", "asWebsideJson"],
		"String": ["asWebsideJson"],
		"RPackage": ["asWebsideJson"],
		"RBMethodProtocolChange": ["websideType"],
		"Context": ["asWebsideJson"],
		"SyntaxErrorNotification": ["asWebsideJson"],
		"RBAssignmentNode": ["asWebsideJson"],
		"ClassDescription": ["asWebsideJson"],
		"RBCommentChange": ["websideType"],
		"RBRefactoryChange": [
			"fromWebsideJson:",
			"websideType",
			"acceptsWebsideJson:",
			"classForWebsideJson:"
		],
		"RBRefactoryClassChange": ["fromWebsideJson:", "asWebsideJson"],
		"CompiledMethod": ["asWebsideJson"],
		"RBRenameVariableChange": ["fromWebsideJson:", "asWebsideJson"],
		"RBMessageNode": ["asWebsideJson"],
		"RBSequenceNode": ["asWebsideJson"],
		"RBRefactoryDefinitionChange": ["asWebsideJson"],
		"RBMethodRefactoring": ["fromWebsideJson:", "asWebsideJson"],
		"RBLiteralArrayNode": ["asWebsideJson"],
		"RGMethodDefinition": ["asWebsideJson"],
		"RBRefactoryVariableChange": ["fromWebsideJson:", "asWebsideJson"],
		"RBRenameClassChange": ["websideType"],
		"RBNode": ["websideType"],
		"Collection": ["asWebsideJson"],
		"RBRemoveProtocolChange": ["websideType"],
		"RBAddClassVariableChange": ["websideType"],
		"RBRemoveClassChange": ["websideType"],
		"RBRemoveInstanceVariableChange": ["websideType"],
		"RBReplaceMethodRefactoring": ["acceptsWebsideJson:"],
		"RBAddMethodRefactoring": ["websideType", "acceptsWebsideJson:"],
		"RBPushDownInstanceVariableRefactoring": ["websideType"],
		"RBAddInstanceVariableChange": ["websideType"],
		"RBRenameInstanceVariableChange": ["websideType"]
	},
	"categories": [
		{
			"name": "Extensions",
			"package": "Webside"
		},
		{
			"name": "Base",
			"package": "Webside"
		},
		{
			"name": "Manifest",
			"package": "Webside"
		}
	]
}
```
