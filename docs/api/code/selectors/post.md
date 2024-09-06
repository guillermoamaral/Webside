# Find selector in source code

This endpoint allows to find a selector in a given pice of source code.

**URL**: `/selectors`

**Method**: `POST`

**Payload**:

```json
{
	"source": "string",
	"position": "number"
}
```

## Success Responses

**Code** : `200 OK`
