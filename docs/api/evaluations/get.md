# Retrieve evaluations

Retrieve active evaluations, i.e., evaluations initiated as explained [here](post.md), and that are being processed at the moment of making this request.

**URL**: `/evaluations`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[evaluation]` where `evaluation` is defined as:

```json
{
	"id": "string",
	"expression": "string",
	"state": "string"
}
```

**Example:**: `GET /evaluations`

```json
[
	{
		"id": "1",
		"expression": "Delay wait: 10000",
		"state": "pending"
	}
]
```
