# Retrieve profiler tree results

Retrieve tree results from the profiler with a given ID.

This tree structure reflects how the time spent is distributed among the different methods.\
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

**URL**: `/profilers/{id}/tree`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Example:**: `GET /profilers/1/tree` (subchildren are omitted to avoid a senseless long example)

```json
{
	"name": "100% (157) 10 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
	"method": "10 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
	"value": 100,
	"children": [
		{
			"name": "54% (85) OrderedCollection>>do:",
			"method": "OrderedCollection>>do:",
			"value": 54,
			"children": []
		},
		{
			"name": "46% (72) Species>>allSubclasses",
			"method": "Species>>allSubclasses",
			"value": 46,
			"children": []
		}
	]
}
```
