# Retrieve changes
Retrieve all changes made to the system in the current session.    

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
* `"MethodClassification"`
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
* `"ClassVariableAddition"`
* `"ClassVariableRename"`
* `"ClassVariableRemove"`
* `"CategoryRename"`
* `"CategoryRemove"`

**Example:**: retrieve changes made by `guille`, `GET /changes?author=guille`.
```json
[
    {
        "type": "MethodDefinition",
        "label": "Float class ≫ phi",
        "project": "Default",
        "timestamp": "2020-12-05T19:13:44.166-03:00",
        "author": "guille",
        "sourceCode": "phi\r\t^1.0 + 5.0 sqrt / 2.0",
        "class": "Float class",
        "selector": "phi",
        "category": "constants"
    }
]
```