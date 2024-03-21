# Apply a change

Apply a change to the system.
This is one of the most important endpoints as it centralizes every change the user can apply to the system.

**Important**. The API specifies a set of _basic_ changes applicable to all dialect analized. However, this list can be extended to support more changes and refactorings by means of [/extensions](../extensions/get.md) endpoint.

Special attention must be paid to the way the target system implementing the API must handle compilation errors so Webside ID can react properly (see [Errors](#errors) below).

**URL**: `/changes`

**Method**: `POST`

**Body**: The body will contain the change to be applied in JSON format. The table below lists all supported changes at this moment. Though types are self-descripted, a short description of what they do is included.
All changes should include `author` proprerty and might specify a `package` property indicating the package in which the change should be applied (for example, the package that will contain a new class).

|           Type           | Description                                                            | Payload                                                                                                                                |
| :----------------------: | ---------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
|        AddMethod         | Define a method within a given class and category.                     | <pre>{<br> "type": "AddMethod",<br> "className": "string",<br> "category": "string",<br> "sourceCode": "string"<br>} </pre>            |
|       RemoveMethod       | Remove a given method                                                  | <pre>{<br> "type": "RemoveMethod",<br> "className": "string",<br> "selector": "string"<br>} </pre>                                     |
|      ClassifyMethod      | Classify a given method under a given category                         | <pre>{<br> "type": "ClassifyMethod",<br> "className": "string",<br> "selector": "string",<br> "category": "string"<br>} </pre>         |
|       RenameMethod       | Rename a given selector. The scope is the whole system.                | <pre>{<br> "type": "RenameMethod",<br> "className": "string",<br> "selector": "string",<br> "newSelector": "string"<br>} </pre>        |
|         AddClass         | Define a new class or changes the definition of an existing one.       | <pre>{<br> "type": "AddClass",<br> "className": "string",<br> "definition": "string"<br>} </pre>                                       |
|       CommentClass       | Change the comment of a given class.                                   | <pre>{<br> "type": "CommentClass",<br> "className": "string",<br> "comment": "string"<br>} </pre>                                      |
|       RemoveClass        | Remove a given class from the system.                                  | <pre>{<br> "type": "RemoveClass",<br> "className": "string"<br>} </pre>                                                                |
|       RenameClass        | Rename a given class.                                                  | <pre>{<br> "type": "RenameClass",<br> "className": "string",<br> "newName": "string",<br> "renameReferences": "boolean"<br>} </pre>    |
|   AddInstanceVariable    | Add a new instance variable to a given class.                          | <pre>{<br> "type": "AddInstanceVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                              |
|  RemoveInstanceVariable  | Remove an instance variable from a given class.                        | <pre>{<br> "type": "RemoveInstanceVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                           |
|  RenameInstanceVariable  | Rename an instance variable of a given class.                          | <pre>{<br> "type": "RenameInstanceVariable",<br> "className": "string",<br> "variable": "string",<br> "newName": "string"<br>} </pre>  |
|  MoveUpInstanceVariable  | Move an instance variable from a given class to its superclass.        | <pre>{<br> "type": "MoveUpInstanceVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                           |
| MoveDownInstanceVariable | Move an instance variable from a given class to one of its subclasses. | <pre>{<br> "type": "MoveDownInstanceVariable",<br> "className": "string",<br> "variable": "string",<br> "target": "string"<br>} </pre> |
|     AddClassVariable     | Add a new class variable to a given class.                             | <pre>{<br> "type": "AddClassVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                                 |
|   RemoveClassVariable    | Remove a class variable from a given class.                            | <pre>{<br> "type": "RemoveClassVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                              |
|   RenameClassVariable    | Rename a class variable of a given class.                              | <pre>{<br> "type": "RenameClassVariable",<br> "className": "string",<br> "variable": "string",<br> "newName": "string"<br>} </pre>     |
|      RenameCategory      | Rename a category within a class.                                      | <pre>{<br> "type": "RenameCategory",<br> "className": "string",<br> "category": "string",<br> "newName": "string"<br>} </pre>          |
|      RemoveCategory      | Remove a category from a class.                                        | <pre>{<br> "type": "RemoveCategory",<br> "className": "string",<br> "category": "string"<br>} </pre>                                   |
|        AddPackage        | Add a new package with a given name.                                   | <pre>{<br> "type": "AddPackage",<br> "name": "string"<br>} </pre>                                                                      |
|      RemovePackage       | Remove a given package                                                 | <pre>{<br> "type": "RemovePackage",<br> "name": "string"<br>} </pre>                                                                   |
|      RenamePackage       | Rename a given package.                                                | <pre>{<br> "type": "RenamePackage",<br> "name": "string",<br> "newName": "string"<br>} </pre>                                          |

## Success Responses

**Code** : `200 OK`

**Content**: the `change` applied (see [get](get.md) to see the structure of a change).
The change is validated before being applied and updated with some information afterwards, thus, the change returned contains more information. For instance, one of the common properties added to the change is `timestamp`, corresponding to the moment at which the change is applied.
There are some special cases like the `selector` property in a `AddMethod`. This property is not required as it is determined by the `sourceCode` property. However, this property is filled by the server and returned to the client.

## Errors

Besides the internal errors in the server (HTTP code `500`), changes might result in a compilation error. These errors should be reported with code `409` with a payload having the following aspect:

```json
{
	"description": "string",
	"fullDescription": "string",
	"interval": {
		"start": "number",
		"end": "number"
	},
	"suggestions": ["suggestion"]
}
```

Here, `suggestion` has the following shape:

```json
{
	"description": "string",
	"changes": ["change"]
}
```

where `changes` is the set of changes that can be applied to mitigate the reported compilation error.

For example, the following error is returned after trying to compile (via a `AddMethod`) the method `m` in `Number` with the source `^1 + `. Note that the interval corresponds the the missing part and there is no suggestion.

```json
409
{
	"description": "primary missing",
	"fullDescription": "primary missing",
	"interval": {
		"start": 7,
		"end": 7
	}
}
```

Another example with suggestions that Webside provides to the user in the form of questions.
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
	"suggestions": [
		{
			"description": "Declare 't' as a temporary",
			"changes": [
				{
					"type": "AddMethod",
					"author": "guille",
					"sourceCode": "m\r\t | t | \r\tt := 1",
					"className": "Number",
					"selector": "m",
					"category": "arithmetic"
				}
			]
		}
	]
}
```

Note that `changes` contains a list with another `AddMethod` with a modified source, which corresponds to accepting the suggestion.

Note also that in the case that the original source had more than one compilation error with potential suggestions, they will be handled one at a time, asking the user to choose what to do for each one. After a first attempt, and having received an error with a list of suggestions, Webside will ask the user; should the user accept one of the proposed suggestions, it will retry with the suggested changes, and if the server finds a new error, the process will repeat.

**Example:**: compile method `phi` in `Float`
`POST /changes`

```json
{
	"type": "AddMethod",
	"className": "Float class",
	"category": "constants",
	"sourceCode": "phi\r\t^1.0 + 5.0 sqrt / 2.0",
	"author": "guille"
}
```
