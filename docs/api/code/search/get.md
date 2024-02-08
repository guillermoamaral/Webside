# Search

This endpoint serves for searching the system for code elements (selectors, methods, classes, packages, etc.).

**URL**: `/search`

**Method**: `GET`

**Query Options**

| Option     |  Type   | Description                                                            |
| ---------- | :-----: | ---------------------------------------------------------------------- |
| text       | string  | The text to compare with                                               |
| ignoreCase | boolean | Whether to ignore the case when comparing. `false` by default          |
| position   | string  | Either `beginning` (default), `including` or `ending`                  |
| type       | string  | Either `all` (default), `selector`, `class`, `package`, or `pool` |

## Success Responses

**Code** : `200 OK`

**Content**: `[result]` where `result` is defined as:

```json
{
	"type": "string",
	"text": "string"
}
```

Where `type` is the type of the element found and can be either `project`, `class`, `selector`, `method` or `pool`.

**Example 1:**: every element starting with "asInt" without taking care of the case, `GET /search?text=asInt&ignoreCase=true`.

```json
[
	{
		"type": "selector",
		"text": "asInteger"
	},
	{
		"type": "selector",
		"text": "asInterval"
	},
	{
		"type": "selector",
		"text": "asInternationalTimestamp"
	}
]
```

**Example 2:**: every class whose name ends with "ix", `GET /search?text=x&position=ending&type=classes`.

```json
[
	{
		"type": "class",
		"text": "FlatMatrix"
	},
	{
		"type": "class",
		"text": "ByteMatrix"
	},
	{
		"type": "class",
		"text": "ColorMatrix"
	},
	{
		"type": "class",
		"text": "FloatMatrix"
	},
	{
		"type": "class",
		"text": "OpenCLMatrix"
	},
	{
		"type": "class",
		"text": "CorrelationMatrix"
	},
	{
		"type": "class",
		"text": "InstructionPrefix"
	},
	{
		"type": "class",
		"text": "IncompleteGammaPrefix"
	}
]
```
