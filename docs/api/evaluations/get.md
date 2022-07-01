# Retrieve evaluations

Retrive active evaluations, i.e., evaluations initiated as explained [here](post.md), and that are being processed at the moment of making this request.

**URL**: `/evaluations`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[evaluation]` where `evaluation` is defined as:

```json
{
	"id": "string",
	"expression": "string"
}
```

**Example:**: `GET /evaluations`

```json
[
	{
		"id": "{207CDBB1-6311-4503-A066-1A89B39A1465}",
		"expression": "Delay wait: 10000"
	}
]
```
