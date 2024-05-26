# Stats

This endpoint provides system stats, usually related to the VM such as GC spaces stats.

**URL**: `/stats`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `string`

**Example:**: Pharo stats `GET /stats`.

```json
[
    {
        "label": "oldSpace",
        "value": 173088768,
        "unit": "bytes"
    },
    {
        "label": "youngSpaceSize",
        "value": 9047464,
        "unit": "bytes"
    },
    {
        "label": "memorySize",
        "value": 196124672,
        "unit": "bytes"
    },
    {
        "label": "freeSize",
        "value": 29363720,
        "unit": "bytes"
    }
]
```
