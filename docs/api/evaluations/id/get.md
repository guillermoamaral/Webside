# Retrieve evaluation

Retrieve the active evaluation with a given ID.

**URL**: `/evaluations/{id}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `evaluation` defined as:

```json
{
	"id": "string",
	"expression": "string",
	"state": "string"
}
```

Where

-   `state` is any of these: `pending`, `evaluating`, `paused`, `failed`, `cancelled` or `finished`.

**Example:**: `GET /evaluations/1`

```json
{
	"id": "1",
	"expression": "1000000 factorial",
	"state": "pending"
}
```
