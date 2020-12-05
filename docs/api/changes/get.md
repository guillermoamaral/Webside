# Retrieve changes
Retrieve all changes made to the system in its current session.    

**URL**: `/changes`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| ------------- | ------------- | ------------- |
| author | string | to get only changes made by an author |

## Success Responses

**Code** : `200 OK`

**Content**: `[change]` where `change` can be one of the following:
```json
{
    "type": "string",
    "label": "string",
    "project": "string",
    "timestamp": "string",
    "author": "string",
    "sourceCode": "string"
}
```
And `type` can be one of the following: 

* `"MethodDefinition"`
* `"MethodRemove"`
* `"SelectorRename"` 
* `"ClassDefinition"`
* `"ClassCommentDefinition"`
* `"ClassRemove"`
* `"ClassRename"`
* `"InstanceVariableAddition"`
* `"InstanceVariableRename"`
* `"InstanceVariableRemove"`
* `"InstanceVariableMoveUp"`
* `"InstanceVariableMoveDown"`
* `"CategoryRename"`
* `"CategoryRemove"`

**Example:**: `GET /changes`.
```json
[

]
```