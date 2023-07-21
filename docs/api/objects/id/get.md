# Retrieve an object

Retrive the pinned object with a given ID.

**URL**: `/objects/{id}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `object` defined as:

```json
{
	"id": "string",
	"class": "string",
	"hasNamedSlots": "boolean",
	"hasIndexedSlots": "boolean",
	"size": "number",
	"printString": "string"
}
```

**Example:**: get the object with the ID `1`, `GET /objects/1`:

```json
{
	"id": "1",
	"class": "Rectangle",
	"hasNamedSlots": true,
	"hasIndexedSlots": false,
	"size": 0,
	"printString": "1 @ 2 rightBottom: 11 @ 12"
}
```
