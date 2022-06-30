# Retrieve test run results

Retrive the results of the test run with a given ID.

**URL**: `/test-runs/{id}/results`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[result]`, where `result` is defined as:

```json
{
	"class": "string",
	"selector": "string",
	"time": "number",
	"type": "type"
}
```

Where `type` can be either `"passed"`, `"failed"`, `"skipped"`, `"error"` or `"knownIssue"`, and `time` is the time required for the test to run.

**Example:**: `GET /test-runs/0a582b87-ea9e-0d00-9d9f-7faf0f32810b/results`

```json
[
	{
		"class": "DateTest",
		"selector": "testAddDays",
		"time": 0,
		"type": "passed"
	},
	{
		"class": "DateTest",
		"selector": "testAddMonths",
		"time": 0,
		"type": "failed"
	}
]
```
