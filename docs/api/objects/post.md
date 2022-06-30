# Pin objects

Pin the object reached thru the given URI.

See [GET](get.md) for details on how to form this URI.

**URL**: `/objects/{id}/*`

**Method**: `POST`

## Success Responses

**Code** : `200 OK`

**Content**: `object` defined as:

```json
{
	"id": "string",
	"class": "string",
	"indexable": "boolean",
	"size": "number",
	"printString": "string"
}
```

**Example:**: Lets suppose that there is a rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`. Requesting a `POST /objects` with the following body, will pin the `x` coordinate of the rectangle's `origin`:

**Payload**

```json
{
	"uri": "/objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/origin/x"
}
```
