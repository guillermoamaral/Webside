# Create a debugger

Create a debugger on a given process. The process is identified thru the evaluation (ID) that created it.

**URL**: `/debuggers`

**Method**: `POST`

**Body**: an `expression` ID:

```json
{
	"evaluation": "string"
}
```

## Success Responses

**Code** : `200 OK`

**Content**:

```json
{
	"id": "string",
	"description": "string"
}
```
