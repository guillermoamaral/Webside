# Retrieve categories

Retrieve categories of a given class.

**URL**: `/class/{name}/categories`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[string]`

**Example:**: `Fraction` categories `GET /classes/Fraction/categories`.

```json
[
	"truncation and round off",
	"comparing",
	"*Math-Operations-Extensions",
	"private",
	"self evaluating",
	"printing",
	"arithmetic",
	"*ston-core",
	"testing",
	"converting"
]
```
