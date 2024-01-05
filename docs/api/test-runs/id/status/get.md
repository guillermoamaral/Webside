# Retrieve test run status

Retrieve the status of the test run with a given ID.

This object allows to provide feedback to the user on how an active test run is progressing. It indicates not only the test being executed at a given time, but the current summary of `"passed"`, `"failed"`, `"errors"`, `"sikkped"`, and `"knownIssues"` tests.

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
		"errors": "number",
		"skipped": "number",
		"knownIssues": "number"
	}
}
```

**Example:**: `GET /test-runs/1/status`

```json
{
	"id": "1",
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
		"errors": 0,
		"skipped": 0,
		"knownIssues": 0
	}
}
```
