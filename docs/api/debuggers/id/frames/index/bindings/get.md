# Retrieve frame bindings

Retrieve bindings of the frame at a given index, within the debugger with a given ID.

**URL**: `/debuggers/{id}/frames/{index}/bindings`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[binding]` where `binding` is defined as:

```json
[
	{
		"name": "string",
		"type": "string",
		"value": "string"
	}
]
```

Where `name` corresponds to the name in the AST, and `type` can be `temporary`, `argument` or `variable` (for pseudo variables, instance variables, class variables and globals).

**Example:**: bindings of frame 1 in debugger with ID `1`, `GET /debugger/1/frames/1/bindings`

```json
[
	{
		"name": "self",
		"value": "1 @ 2",
		"type": "variable"
	},
	{
		"name": "aPoint",
		"value": "10 @ 20",
		"type": "argument"
	}
]
```
