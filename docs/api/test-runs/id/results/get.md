# Retrieve test run results

Retrieve the results of the test run with a given ID.

**URL**: `/test-runs/{id}/results`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: a `result` defined as:

```json
{
    "passed": ["test"],
    "failed": ["test"],
    "errors": ["test"],
    "skipped": ["test"],
    "knownIssues": ["test"]
}
```

Where `test` is defined as:

```json
{
    "class": "string",
    "selector": "string",
    "time": "number"
}
```

**Example:**: `GET /test-runs/1/results`

```json
{
    "class": "TestResult",
    "passed": [
        {
            "class": "DateTest",
            "selector": "testAccessing",
            "time": 0
        },
        {
            "class": "DateTest",
            "selector": "testAddDays",
            "time": 0
        }
    ],
    "failed": [
        {
            "class": "DateTest",
            "selector": "testPrintFormat",
            "time": 0
        }
    ],
    "errors": [],
    "skipped": [],
    "knownIssues": []
}
```
