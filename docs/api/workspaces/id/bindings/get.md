# Retrieve workspace bindings

Retrieve bindings of the workspace with a given ID.

**URL**: `/workspaces/{id}/bindings`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[binding]` where `binding` is defined as:

```json
[
	{
		"name": "string",
		"value": "string"
	}
]
```

**Example:**: bindings of workspaces with ID `1`, `GET /workspace/1/bindings`

```json
[
	{
		"name": "p1",
		"value": "1 @ 2"
	},
	{
		"name": "p2",
		"value": "10 @ 20"
	}
]
```
