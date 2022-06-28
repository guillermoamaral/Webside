# Retrieve test run status

Retrive the status of the test run with a given ID.

This object allows to provide feedback to the user on how an active test run is progressing. It indicates not only the test being executed at a given time, but the current summary of `passed`, `failed`, `error`, `sikkped`, and `knownIssues` tests.

**URL**: `/test-runs/{id}/status`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `status` defined as:

```json
{
	"id": "string",
	"name": "string",
	"total": 1,
	"running": false,
	"current": "signature",
	"summary": {
		"run": "number",
		"passed": "number",
		"failed": "number",
		"error": "number",
		"skipped": "number",
		"knownIssue": "number"
	}
}
```

Where `type` can be `passed`, `failed`, `skipped`, `error` or `knownIssue`, and `time` is the time required for the test to run.

**Example:**: `GET /test-runs/0a582b87-ea9e-0d00-9d9f-7faf0f32810b/status`

```json
{
	"id": "{6AC24F85-594A-4C46-A6CF-410404998AB4}",
	"class": "TestRunner",
	"name": "DateTest >> #testAdd",
	"total": 1,
	"running": false,
	"current": {
		"class": "DateTest",
		"selector": "testAdd",
		"time": 0
	},
	"summary": {
		"run": 1,
		"passed": 1,
		"failed": 0,
		"error": 0,
		"skipped": 0,
		"knownIssue": 0
	}
}
```
