# Retrieve active processes

Retrieve processe currently active in the system.

**URL**: `/processes`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[process]` where `process` is defined as:

```json
{
	"id": "string"
}
```

**Example:**: `GET /processes`

```json
[
	[
		{
			"id": "1",
			"printString": "Webside evaluation 1 Process - [Sleeping] (basicHash: 3212 priority: 4)"
		}
	]
]
```
