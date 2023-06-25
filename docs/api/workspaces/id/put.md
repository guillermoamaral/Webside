# Update a workspace

Update the workspace with a given ID.

**URL**: `/workspaces/{id}`

**Method**: `PUT`

## Success Responses

**Code** : `200 OK`

**Example:**: `GET /workspaces/1`

**Body**: a `workspace` with the following properties:

```json
{
	"source": "string"
}
```

**Example:**: `PUT /workspaces/1` with the following body:

```json
{
	"source": "4 + 5"
}
```

Response:

```json
{
	"id": 1,
	"source": "4 + 5"
}
```
