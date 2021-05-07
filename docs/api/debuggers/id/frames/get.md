# Retrieve debugger frames
Retrieve frames of the debugger with a given ID.

**URL**: `/debuggers/{id}/frames`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[frame]` where `frame` is defined as:
```json
[
    {
        "index": "number",
        "label": "string",
    }
]
```
_Note: though frame representation includes more properties (see [/debuggers/{id}/frames/{index}](id/frames/index/get.md)), this endpoint is required to provide just `index` and `label`_ 


**Example:**: frames of debugger with ID `{7B9C472C-376F-493A-9B88-5CCBA70A4309}`, `GET /debugger/{7B9C472C-376F-493A-9B88-5CCBA70A4309}/frames`
```json
[
	{
		"index": 1,
		"label": "SmallInteger(Integer)>>factorial"
	},
	{
		"index": 2,
		"label": "SmallInteger(Integer)>>factorial"
	},
	{
		"index": 3,
		"label": "UndefinedObject>>doit"
	}
]
```