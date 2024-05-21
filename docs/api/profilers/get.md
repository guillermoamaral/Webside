# Retrieve active profilers

Retrieve active profilers.

**URL**: `/profilers`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

```json
[
	{
		"id": "1",
		"expression": "100 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
		"state": "finished"
	},
	{
		"id": "2",
		"expression": "10 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
		"state": "finished"
	}
]
```
