# Retrieve object instance variables
Retrive instance variables of pinned object (or one of its descendants see ([/objects/{id}/*](../slots/get.md))).

**URL**: `/objects/{id}/instance-variables` (or `/objects/{id}/*/instance-variables`)

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[variable]` where `variable` is defined as:
```json
{
    "name": "string",
    "class": "string"
}
```

**Example:**: instance variables the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/instance-variables`
```json
[
    {
        "name": "origin",
        "class": "Rectangle"
    },
    {
        "name": "corner",
        "class": "Rectangle"
    }
]
```