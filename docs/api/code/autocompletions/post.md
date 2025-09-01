# Autocompletions

This endpoint is aim to provide a list of completion entries for given a piece of code, position and context (class).

**URL**: `/autocompletions`

**Method**: `POST`

**Payload**: The payload should specify this information like this:

```json
{
	"source": "string",
	"position": "integer",
	"class": "string",
	"inMethod": "boolean"
}
```

where `class` property is optional.

## Success Responses

**Code** : `200 OK`

**Content**: A list of completion entries, where each entry should have the following structure:

```json
{
	"label": "string",
	"type": "string",
	"detail": "string"
}
```

where `type` is either `class`, `method`, `variable` or `keyword` (used for pseudo variables, `true`, `false` and `nil`).
The IDE will use this information to style the list shown to the user.

**URL**: `/autocompletions`

**Method**: `POST`

## Success Responses

**Code** : `200 OK`

**Example:**: completion entries for the following code, position and class.

```json
{
	"class": "Rectangle",
	"source": "m\r  self sc",
	"position": 11
}
```

```json
[
	{
		"label": "scaleBy:",
		"type": "variable"
	},
	{
		"label": "scaleTo:",
		"type": "variable"
	}
]
```
