# Apply a change

Apply a change to the system.
This is one of the most important endpoints as it centralizes every change the user can apply to the system.

**Important**. The API specifies a set of _basic_ changes applicable to all dialect analized. However, this list can be extended to support more changes and refactorings by means of [/extensions](../extensions/changes.md) endpoint.

Special attention must be paid to the way the target system must handle compilation errors so Webside IDE can react properly (see [Errors](#errors) below).

**URL**: `/changes`

**Method**: `POST`

**Payload**: The payload will contain the change to be applied in JSON format. The table below lists all supported changes at this moment. Though types are self-descripted, a short description of what they do is included.
All changes should include `author` proprerty and might specify a `package` property indicating the package in which the change should be applied (for example, the package that will contain a new class).

|           Type           | Description                                                            | Payload                                                                                                                                                                                                                                   |
| :----------------------: | ---------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|        AddMethod         | Compile a method in a given class.                                     | <pre>{<br> "type": "AddMethod",<br> "package": "string",<br> "className": "string",<br> "category": "string",<br> "sourceCode": "string"<br>} </pre>                                                                                      |
|       RemoveMethod       | Remove a given method.                                                 | <pre>{<br> "type": "RemoveMethod",<br> "className": "string",<br> "selector": "string"<br>} </pre>                                                                                                                                        |
|      ClassifyMethod      | Classify a given method under a given category.                        | <pre>{<br> "type": "ClassifyMethod",<br> "className": "string",<br> "selector": "string",<br> "category": "string"<br>} </pre>                                                                                                            |
|       RenameMethod       | Rename a given selector. The scope is the whole system.                | <pre>{<br> "type": "RenameMethod",<br> "className": "string",<br> "selector": "string",<br> "newSelector": "string"<br>} </pre>                                                                                                           |
|         AddClass         | Define a new class or change the definition of an existing one.        | <pre>{<br> "type": "AddClass",<br> "className": "string",<br> "package": "string",<br> "definition": "string" [^1],<br> "instanceVariables": ["string"],<br> "classVariables": ["string"],<br> "poolDictionaries": ["string"]<br>} </pre> |
|       CommentClass       | Change the comment of a given class.                                   | <pre>{<br> "type": "CommentClass",<br> "className": "string",<br> "comment": "string"<br>} </pre>                                                                                                                                         |
|       RemoveClass        | Remove a given class.                                                  | <pre>{<br> "type": "RemoveClass",<br> "className": "string"<br>} </pre>                                                                                                                                                                   |
|       RenameClass        | Rename a given class.                                                  | <pre>{<br> "type": "RenameClass",<br> "className": "string",<br> "newName": "string",<br> "renameReferences": "boolean"<br>} </pre>                                                                                                       |
|     AddClassCategory     | Add a new class category.                                              | <pre>{<br> "type": "AddClassCategory",<br> "package": "string",<br> "category": "string"<br>} </pre>                                                                                                                                      |
|   RenameClassCategory    | Rename a class category.                                               | <pre>{<br> "type": "RenameClassCategory",<br> "package": "string",<br> "category": "string",<br> "newName": "string"<br>} </pre>                                                                                                          |
|   RemoveClassCategory    | Remove a class category.                                               | <pre>{<br> "type": "RemoveClassCategory",<br> "package": "string",<br> "category": "string"<br>} </pre>                                                                                                                                   |
|   AddInstanceVariable    | Add a new instance variable to a given class.                          | <pre>{<br> "type": "AddInstanceVariable",<br> "package": "string",<br> "variable": "string"<br>} </pre>                                                                                                                                   |
|  RemoveInstanceVariable  | Remove an instance variable from a given class.                        | <pre>{<br> "type": "RemoveInstanceVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                                                                                                                              |
|  RenameInstanceVariable  | Rename an instance variable of a given class.                          | <pre>{<br> "type": "RenameInstanceVariable",<br> "className": "string",<br> "variable": "string",<br> "newName": "string"<br>} </pre>                                                                                                     |
|  MoveUpInstanceVariable  | Move an instance variable from a given class to its superclass.        | <pre>{<br> "type": "MoveUpInstanceVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                                                                                                                              |
| MoveDownInstanceVariable | Move an instance variable from a given class to one of its subclasses. | <pre>{<br> "type": "MoveDownInstanceVariable",<br> "className": "string",<br> "variable": "string",<br> "target": "string"<br>} </pre>                                                                                                    |
|     AddClassVariable     | Add a new class variable to a given class.                             | <pre>{<br> "type": "AddClassVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                                                                                                                                    |
|   RemoveClassVariable    | Remove a class variable from a given class.                            | <pre>{<br> "type": "RemoveClassVariable",<br> "className": "string",<br> "variable": "string"<br>} </pre>                                                                                                                                 |
|   RenameClassVariable    | Rename a class variable of a given class.                              | <pre>{<br> "type": "RenameClassVariable",<br> "className": "string",<br> "variable": "string",<br> "newName": "string"<br>} </pre>                                                                                                        |
|      RenameCategory      | Rename a category within a class.                                      | <pre>{<br> "type": "RenameCategory",<br> "className": "string",<br> "category": "string",<br> "newName": "string"<br>} </pre>                                                                                                             |
|      RemoveCategory      | Remove a category from a class.                                        | <pre>{<br> "type": "RemoveCategory",<br> "className": "string",<br> "category": "string"<br>} </pre>                                                                                                                                      |
|        AddPackage        | Add a new package.                                                     | <pre>{<br> "type": "AddPackage",<br> "name": "string"<br>} </pre>                                                                                                                                                                         |
|      RemovePackage       | Remove a given package                                                 | <pre>{<br> "type": "RemovePackage",<br> "name": "string"<br>} </pre>                                                                                                                                                                      |
|      RenamePackage       | Rename a given package.                                                | <pre>{<br> "type": "RenamePackage",<br> "name": "string",<br> "newName": "string"<br>} </pre>                                                                                                                                             |

[^1]: Property `definition` is highly dependent on the backend and can be substituted by properties `instanceVariables`, `classVariables` and `poolDictionaries`.

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

## Success Responses

**Code** : `200 OK`

**Content**: the `change` applied (see [get](get.md) to see the structure of a change).
The change is validated before being applied and updated with some information afterwards, thus, the change returned contains more information. For instance, one of the common properties added to the change is `timestamp`, corresponding to the moment at which the change is applied.
There are some special cases like the `selector` property in a `AddMethod`. This property is not required as it is determined by the `sourceCode` property. However, this property is filled by the server and returned to the client.

## Errors

Whenever a change cannot be appplied, the backend should respond with an client error code (typically `409`), indicating what went wrong using the `description` property.\
Also, the backend might provide one or more _suggestions_ on how to carry on the intended change.
These should be variations of the original change, together with a description that could be used by the IDE to prompt the user what they want to do.
This is the aspect of an error response data.

```json
{
  "description": "string",
  "suggestions": ["suggestion"]
}
```

Where, `suggestion` has the following shape:

```json
{
  "description": "string",
  "changes": ["change"]
}
```

and `changes` is the set of changes that will be applied to mitigate the reported error.

Note that this is a list as it would be necessary to apply several changes to accomplish what the client wanted to do (i.e., the original change). This is better understood by an example like the one below.

### Compilation Errors

As explained above, these should be reported with code `409`, a `description` and eventually a list of `suggestions`. In addition to that information, compilation errors should be reported with a more detail description, `fullDescription`, and the `interval` in the source code where the error was detected. This would be the structure of such a response:

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

For example, after sending a `AddMethod` change on `Number` class with the source code `^1 + `, the the following error is returned:

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

Note that the interval corresponds the the missing part and there are no suggestions.

Here is another example with suggestions: Lets suppose we try to compile a method with a single line `t := 1` on a class where `t` is not in the scope.
The error returned should look like this:

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

Note also that the original source could contain more than one error. These should be presented one at a time, offering suggestions for each error, applying the changes if the user chooses one, and repeating cycle for every error found in the process.\
For instance, if the code sent in an `AddMethod` change is the following `m t1 := 1. t2 := 2`, and neither `t1` and `t2` are defined, a first response will be like before:

```json
409
{
    "description": "undeclared t1",
    "fullDescription": "undeclared identifier t1 at line 2 position 3",
    "interval": {
        "start": 5,
        "end": 6
    },
    "suggestions": [
        {
            "description": "Declare 't1' as a temporary",
            "changes": [
                {
                    "type": "AddMethod",
                    "author": "guille",
                    "sourceCode": "m\r   | t1 | \r\tt1 := 1.\r  t2 := 2.",
                    "className": "Number",
                    "category": "arithmetic"
                }
            ]
        }
    ]
}
```

And once the user choses the suggestion, a new error is generated indicating `t2` is not defined, but this time based on the code of the previously accepted suggestion.

```json
409
{
    "description": "undeclared t2",
    "fullDescription": "undeclared identifier t2 at line 4 position 3",
    "interval": {
        "start": 26,
        "end": 27
    },
    "suggestions": [
        {
            "description": "Declare 't2' as a temporary",
            "changes": [
                {
                    "type": "AddMethod",
                    "author": "guille",
                    "sourceCode": "m\r   | t1 t2 | \r\tt1 := 1.\r  t2 := 2.",
                    "className": "Number",
                    "category": "arithmetic"
                }
            ]
        }
    ]
}
```

### Confirmation

Notice that this mechanism can be used to confirm a change on the server side: by just providing a suggestion with the original change (plus a flag with the confirmation) and a description like `Are you sure?`.

This differs from any confirmation on the client side as the backend might have (and surely has) a richer context.
