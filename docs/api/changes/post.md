# Apply a change

Apply a change to the system.
This is one of the most important endpoints as it centralizes every change the user can apply to the system.
Important: the list of possible changes at the moment of writing this documentation is by no means a closed list and it can be extended to support more changes and refactorings.

Special attention must be paid to the way the API should handle compilation errors so Webside can react properly.

**URL**: `/changes`

**Method**: `POST`

**Body**: The body will contain the change to be applied in JSON format. The table below lists all supported changes at this moment. Though types are self-descripted, a short description of what they do is included.
All changes should include `author` proprerty and might specify a `package` property indicating the package in which the change should be applied (for example, the package that will contain a new class).

|           Type           | Description                                                            | Payload                                                                                                                            |
| :----------------------: | ---------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
|        AddMethod         | Define a method within a given class and category.                     | <pre>{<br> "type": "AddMethod",<br> "class": "string",<br> "category": "string",<br> "sourceCode": "string"<br>} </pre>            |
|       RemoveMethod       | Remove a given method                                                  | <pre>{<br> "type": "RemoveMethod",<br> "class": "string",<br> "selector": "string"<br>} </pre>                                     |
|      ClassifyMethod      | Classify a given method under a given category                         | <pre>{<br> "type": "ClassifyMethod",<br> "class": "string",<br> "selector": "string",<br> "category": "string"<br>} </pre>         |
|       RenameMethod       | Rename a given selector. The scope is the whole system.                | <pre>{<br> "type": "RenameMethod",<br> "class": "string",<br> "selector": "string",<br> "newSelector": "string"<br>} </pre>        |
|         AddClass         | Define a new class or changes the definition of an existing one.       | <pre>{<br> "type": "AddClass",<br> "class": "string",<br> "definition": "string"<br>} </pre>                                       |
|       CommentClass       | Change the comment of a given class.                                   | <pre>{<br> "type": "CommentClass",<br> "class": "string",<br> "comment": "string"<br>} </pre>                                      |
|       RemoveClass        | Remove a given class from the system.                                  | <pre>{<br> "type": "RemoveClass",<br> "class": "string"<br>} </pre>                                                                |
|       RenameClass        | Rename a given class.                                                  | <pre>{<br> "type": "RenameClass",<br> "class": "string",<br> "newName": "string",<br> "renameDiferences": "boolean"<br>} </pre>    |
|   AddInstanceVariable    | Add a new instance variable to a given class.                          | <pre>{<br> "type": "AddInstanceVariable",<br> "class": "string",<br> "variable": "string"<br>} </pre>                              |
|  RemoveInstanceVariable  | Remove an instance variable from a given class.                        | <pre>{<br> "type": "RemoveInstanceVariable",<br> "class": "string",<br> "variable": "string"<br>} </pre>                           |
|  RenameInstanceVariable  | Rename an instance variable of a given class.                          | <pre>{<br> "type": "RenameInstanceVariable",<br> "class": "string",<br> "variable": "string",<br> "newName": "string"<br>} </pre>  |
|  MoveUpInstanceVariable  | Move an instance variable from a given class to its superclass.        | <pre>{<br> "type": "MoveUpInstanceVariable",<br> "class": "string",<br> "variable": "string"<br>} </pre>                           |
| MoveDownInstanceVariable | Move an instance variable from a given class to one of its subclasses. | <pre>{<br> "type": "MoveDownInstanceVariable",<br> "class": "string",<br> "variable": "string",<br> "target": "string"<br>} </pre> |
|     AddClassVariable     | Add a new class variable to a given class.                             | <pre>{<br> "type": "AddClassVariable",<br> "class": "string",<br> "variable": "string"<br>} </pre>                                 |
|   RemoveClassVariable    | Remove a class variable from a given class.                            | <pre>{<br> "type": "RemoveClassVariable",<br> "class": "string",<br> "variable": "string"<br>} </pre>                              |
|   RenameClassVariable    | Rename a class variable of a given class.                              | <pre>{<br> "type": "RenameClassVariable",<br> "class": "string",<br> "variable": "string",<br> "newName": "string"<br>} </pre>     |
|      RenameCategory      | Rename a category within a class.                                      | <pre>{<br> "type": "RenameCategory",<br> "class": "string",<br> "category": "string",<br> "newName": "string"<br>} </pre>          |
|      RemoveCategory      | Remove a category from a class.                                        | <pre>{<br> "type": "RemoveCategory",<br> "class": "string",<br> "category": "string"<br>} </pre>                                   |
|        AddPackage        | Add a new pacakge with a given name.                                   | <pre>{<br> "type": "AddPackage",<br> "name": "string"<br>} </pre>                                                                  |
|      RemovePackage       | Remove a given package                                                 | <pre>{<br> "type": "RemovePackage",<br> "name": "string"<br>} </pre>                                                               |
|      RenamePackage       | Rename a given package.                                                | <pre>{<br> "type": "RenamePackage",<br> "name": "string",<br> "newName": "string"<br>} </pre>                                      |

## Success Responses

**Code** : `200 OK`

**Content**: the `change` applied (see [get](get.md) to see the structure of a change).
The change is validated before being applied and updated with some information afterwards, thus, the change returned contains more information. For instance, one of the common properties added to the change is `timestamp`, corresponding to the moment at which the change is applied.
There are some special cases like the `selector` property in a `AddMethod`. This property is not required as it is determined by the `sourceCode` property. However, this property is filled by the server and returned to the client.

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

For example, the following error is returned after trying to compile (via a `AddMethod`) the method `m` in `Number` with the source `^1 + `. Note that the interval corresponds the the missing part and there is no suggestion.

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
			"type": "AddMethod",
			"author": "guille",
			"sourceCode": "m\r\t | t | \r\tt := 1",
			"class": "Number",
			"selector": "m",
			"category": "arithmetic"
		}
	]
}
```

Note that `changes` contains a list with another `AddMethod` with a modified source, which corresponds to accepting the suggestion.

Note also that in the case that the original source had more than one compilation error with potential suggestions, they are handled one by one, asking the user to accept each suggestion at a time (i.c. Webside sends the first attempt and after receiving an error with a suggestion, it asks the user; should the user accept the suggestion, Webside retries with the suggested changes and if the server finds a new error, the process repeats).

**Example:**: compile method `phi` in `Float`
`POST /changes`

```json
{
	"type": "AddMethod",
	"class": "Float class",
	"category": "constants",
	"sourceCode": "phi\r\t^1.0 + 5.0 sqrt / 2.0",
	"author": "guille"
}
```
