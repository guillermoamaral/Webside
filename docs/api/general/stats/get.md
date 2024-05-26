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
        "label": "Old Space",
        "value": 149.0703125,
        "unit": "mb"
    },
    {
        "label": "Young Space",
        "value": 2.9942855834960938,
        "unit": "mb"
    },
    {
        "label": "Total Space",
        "value": 171.0390625,
        "unit": "mb"
    },
    {
        "label": "Free Space",
        "value": 29.71562957763672,
        "unit": "mb"
    }
]
```
