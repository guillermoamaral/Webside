# Retrieve class variables

Retrieve class variables of a given class.

**URL**: `/class/{name}/class-variables`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[variable]` where `variable` is defined as:

```json
{
	"name": "string",
	"class": "string"
}
```

**Example:**: `Fraction` class variables `GET /classes/Fraction/class-variables`.

```json
[
	{
		"name": "DependentsFields",
		"class": "Object class",
		"type": "class"
	}
]
```
