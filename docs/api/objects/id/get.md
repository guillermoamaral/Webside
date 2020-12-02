# Retrieve an object
Retrive a pinned object by ID.

**URL**: `/objects/{id}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[object]` where `object` is defined as:
```json
{
    "id": "string",
    "class": "string",
    "indexable": "boolean",
    "size": "number",
    "printString": "string"
}
]
```

**Example:**: `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}`
```json
{
    "id": "{207CDBB1-6311-4503-A066-1A89B39A1465}",
    "class": "WebsideObject",
    "indexable": false,
    "size": 0,
    "printString": "WebsideObject on 1 @ 2 rightBottom: 11 @ 12"
}
```