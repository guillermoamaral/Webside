# Retrieve objects

Retrieve pinned objects.

These objects are the result of evaluating expressions. As it is described in [/evaluations](../evaluations/post), a synchronous evaluation might pin the resulting object or not, while an asynchronous evaulation pins always the resulting object. In any case, the ID of the object is the same as the one of the corresponding evaluation.

**URL**: `/objects`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[object]` where `object` is defined as:

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

**Example:**: `GET /objects`

```json
[
	{
		"id": "1",
		"class": "Rectangle",
		"hasNamedSlots": true,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "1 @ 2 rightBottom: 11 @ 12"
	}
]
```
