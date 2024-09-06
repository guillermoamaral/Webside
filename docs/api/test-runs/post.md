# Run tests

Create and run a test suite.

**URL**: `/test-runs`

**Method**: `POST`

**Payload**: a `suite` given by:

```json
{
	"packages": ["string"],
	"classes": ["string"],
	"methods": ["signature"]
}
```

Where `signature` is:

```json
{ "class": "string", "selector": "string" }
```

These options can be combined in any way. For example, one single package, class or method, or a combination of them.

## Success Responses

**Code** : `200 OK`

**Payload**: even when the object representing the test run might vary from dialect to dialect, the response should contain the `ID` of the created run in order to follow its progress and request its results:

```json
{
	"id": "string",
	"name": "string"
}
```

Where the `name` is the one given to the run by the backend and can be determined by the created suite (for example, the name of a test class).

**Example:**: running `testAddDays` from `DateTest` class:
`POST /evaluations`

```json
{
	"methods": [
		{
			"class": "DateTest",
			"selector": "testAddDays"
		}
	]
}
```

Response:

```json
{
	"id": "1",
	"name": "DateTest >> #testAddDays"
}
```
