# Pin objects

Pin the object reached thru the given URI.

See [GET](get.md) for details on how to form this URI.

**URL**: `/objects/{id}/*`

**Method**: `POST`

## Success Responses

**Code** : `200 OK`

**Payload**: `object` defined as:

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

**Example:**: Lets suppose there is a rectangle with ID `1`. Requesting a `POST /objects` with the following body, will pin the `x` coordinate of the rectangle's `origin`:

**Payload**

```json
{
	"uri": "/objects/1/origin/x"
}
```
