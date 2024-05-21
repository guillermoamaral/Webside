# Retrieve a profiler

Retrieve the profiler with a given ID.

**URL**: `/profilers/{id}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Example:**: `GET /profilers/1`

```json
{
	"id": "1",
	"expression": "100 timesRepeat: [Object allSubclasses do: [:c | c name size factorial]]",
	"state": "finished"
}
```
