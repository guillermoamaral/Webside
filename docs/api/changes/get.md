# Retrieve changes

Retrieve all changes made to the system in the current session.

**URL**: `/changes`

**Method**: `GET`

**Query Options**

| Option |  Type  | Description                           |
| :----: | :----: | ------------------------------------- |
| author | string | to get only changes made by an author |

## Success Responses

**Code** : `200 OK`

**Content**: `[change]` where `change` can be one of the following:

```json
{
	"type": "string",
	"label": "string",
	"package": "string",
	"timestamp": "string",
	"author": "string",
	"sourceCode": "string"
}
```

And `type` can be one of the following:

- `"AddMethod"`
- `"RemoveMethod"`
- `"ClassifyMethod"`
- `"RenameMethod"`
- `"AddClass"`
- `"CommentClass"`
- `"RemoveClass"`
- `"RenameClass"`
- `"AddInstanceVariable"`
- `"RemoveInstanceVariable"`
- `"RenameInstanceVariable"`
- `"MoveUpInstanceVariable"`
- `"MoveDownInstanceVariable"`
- `"AddClassVariable"`
- `"RemoveClassVariable"`
- `"RenameClassVariable"`
- `"RenameCategory"`
- `"RemoveCategory"`
- `"AddPackage"`
- `"RemovePackage"`
- `"RenamePackage"`

**Example:**: retrieve changes made by `guille`, `GET /changes?author=guille`.

```json
[
	{
		"type": "AddMethod",
		"label": "Float class ≫ phi",
		"package": "Default",
		"timestamp": "2020-12-05T19:13:44.166-03:00",
		"author": "guille",
		"sourceCode": "phi\r\t^1.0 + 5.0 sqrt / 2.0",
		"className": "Float class",
		"selector": "phi",
		"category": "constants"
	}
]
```
