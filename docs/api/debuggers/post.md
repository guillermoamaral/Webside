# Create a debugger

Create a debugger on a given process. The process is identified thru the evaluation (ID) that created it.

**URL**: `/debuggers`

**Method**: `POST`

**Payload**: an `evaluation` (the ID obtained when creating the evaluation):

```json
{
	"evaluation": "string"
}
```

## Success Responses

**Code** : `200 OK`

**Payload**:

```json
{
	"id": "string",
	"description": "string"
}
```
