# Retrieve profiler ranking results

Retrieve ranking results from the profiler with a given ID.

This is the ranking of most executed methods, where each record has the same structure the tree node for the tree results (see [/tree}](../tree/get.md)).\
This is the structure of a `node`:

```json
{
	"label": "string",
	"method": "signature",
	"value": "number",
	"children": ["node"]
}
```

Where,

-   `signature` is the method signature (`class>>selector`), and
-   `value` represents the percent of time in that method

**URL**: `/profilers/{id}/ranking`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Example:**: `GET /profilers/1/ranking` (list is cutted to avoid a senseless long example)

```json
[
	{
		"label": "100% (157) 10 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
		"method": "10 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
		"value": 100,
		"children": []
	},
	{
		"label": "54% (85) Species>>primitiveNewBytes:",
		"method": "Species>>primitiveNewBytes:",
		"value": 54,
		"children": []
	},
	{
		"label": "54% (85) OrderedCollection>>do:",
		"method": "OrderedCollection>>do:",
		"value": 54,
		"children": []
	},
	{
		"label": "54% (85) UndefinedObject>>nil",
		"method": "UndefinedObject>>nil",
		"value": 54,
		"children": []
	},
	{
		"label": "46% (72) Species>>allSubclasses",
		"method": "Species>>allSubclasses",
		"value": 46,
		"children": []
	},
	{
		"label": "46% (72) Species>>allSubclassesDo:",
		"method": "Species>>allSubclassesDo:",
		"value": 46,
		"children": []
	},
	{
		"label": "46% (72) Class>>subclassesDo:",
		"method": "Class>>subclassesDo:",
		"value": 46,
		"children": []
	}
]
```
