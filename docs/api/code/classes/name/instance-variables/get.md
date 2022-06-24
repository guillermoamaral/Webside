# Retrieve instance variables

Retrieve instance variables of a given class.

**URL**: `/class/{name}/instance-variables`

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

**Example:**: `Fraction` instance variables `GET /classes/Fraction/instance-variables`.

```json
[
	{
		"name": "numerator",
		"class": "Fraction",
		"type": "instance"
	},
	{
		"name": "denominator",
		"class": "Fraction",
		"type": "instance"
	}
]
```
