# Apply a change
Apply a change to the system.
This is one of the most important endpoints as it centralizes every change that the user ones to apply to the system.

Pay special attention to the way compilation errors should be handled by the API so Webside can react properly.

**URL**: `/changes`

**Method**: `POST`

**Body**: one of the following:

```json
{
    "type": "MethodDefinition",
    "class": "string",
    "category": "string",
    "sourceCode": "string",
    "author": "string"
}
```

```json
{
    "type": "MethodRemove",
    "class": "string",
    "selector": "string"
}
```

```json
{
    "type": "SelectorRename",
    "class": "string",
    "selector": "string",
    "newSelector": "string"
}
```

```json
{
    "type": "ClassDefinition",
    "class": "string",
    "definition": "string"
}
```

```json
{
    "type": "ClassCommentDefinition",
    "class": "string",
    "comment": "string"
}
```

```json
{
    "type": "ClassRemove",
    "class": "string"
}
```

```json
{
    "type": "ClassRename",
    "class": "string",
    "newName": "string",
    "renameReferences": "boolean"
}
```

```json
{
    "type": "InstanceVariableAddition",
    "class": "string",
    "variable": "string"
}
```

```json
{
    "type": "InstanceVariableRename",
    "class": "string",
    "variable": "string",
    "newName": "string"
}
```

```json
{
    "type": "InstanceVariableRemove",
    "class": "string",
    "variable": "string"
}
```

```json
{
    "type": "InstanceVariableMoveUp",
    "class": "string",
    "variable": "string"
}
```

```json
{
    "type": "InstanceVariableMoveDown",
    "class": "string",
    "variable": "string",
    "target": "string",
}
```

```json
{
    "type": "CategoryRename",
    "class": "string",
    "category": "string",
    "newName": "string",
}
```

```json
{
    "type": "CategoryRemove",
    "class": "string",
    "category": "string"
}
```

## Success Responses

**Code** : `200 OK`

**Content**: the `change` applied.
The change is validated before being applied and updated with some information afterwards, thus, the change returned contains more information. For instance, one of the common properties added to the change is `timestamp`, corresponding to the moment at which the change is applied.
There are some special cases like the `selector` property in a `MethodDefinition`. This property is not required as it is determined by the `sourceCode` property. However, this property is filled by the server and returned to the client.

### Error Codes Details
Besides the internal errors in the server (HTTP code `500`), changes might result in a compilation error. These errors are reported with code `409` with a payload having the following aspect:
```json
{
	"description": "string",
	"fullDescription": "string",
	"interval": {
		"start": "number",
		"end": "number"
	},
	"suggestion": "string",
	"changes": ["change"]
}
```
Here, `suggestion` describes a set of `changes` that can be applied to mitigate the reported compilation error.

For example, the following error is returned after trying to compile (via a `MethodDefinition`) the method `m` in `Number` with the source `^1 + `. Note that the interval corresponds the the missing part and there is no suggestion.

```json
409
{
	"description": "primary missing",
	"fullDescription": "primary missing",
	"interval": {
		"start": 7,
		"end": 7
	},
	"suggestion": null,
	"changes": []
}
```

Another common example where there are suggestions that Webside provides to the user in the form of questions.
Let's say we try to compile a method with a single line assigning `t := 1` in a class where `t` is not defined (it is not an instance variable nor a global).
The error returned should look like: 
```json
409
{
	"description": "undeclared t",
	"fullDescription": "undeclared identifier t at line 2 position 2",
	"interval": {
		"start": 4,
		"end": 4
	},
	"suggestion": "Declare 't' as a temporary",
	"changes": [
		{
			"type": "MethodDefinition",
			"author": "guille",
			"sourceCode": "m\r\t | t | \r\tt := 1",
			"class": "Number",
			"selector": "m",
			"category": "arithmetic"
		}
	]
}
```

Note that `changes` (another `MethodDefinition` with a modified source in this case) correspond to take the suggestion.

Note also that in case the original source had more than one compilation error with potential suggestions, they are handled one by one, prompting the user one suggestion at a time (i.c. Webside sends the first attempt and then prompts back the user; should the user accept the suggestion, Webside retries with suggested changes and if the server finds a new error, Webside asks the user again to accept the new suggestion).

**Example:**: compile method `phi` in `Float`
`POST /changes`

```json
{
    "type": "MethodDefinition",
    "class": "Float class",
    "category": "constants",
    "sourceCode": "phi\r\t^1.0 + 5.0 sqrt / 2.0",
    "author": "guille"
}
```