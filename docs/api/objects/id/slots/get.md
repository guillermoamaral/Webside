# Retrieve object slots
Retrive pinned object slots.

This endpoint contains a _variable_ part (`/*`) in the URI that serves to indicate the path from the root object to the slot of interest by naming each slot in between.

Note that slot names should be known beforehand. Webside, and the inspector in particular, makes a previous request to retrieve the instance variable names of the object class ([/classes/{name}/instance-variables](../../../../classes/instance-variables/get.md)) in order to retrieve object slots. 

**URL**: `/objects/{id}/*`

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

**Example:**: `x` slot of the `corner` of the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/corner/x`
```json
{
    "class": "SmallInteger",
    "indexable": false,
    "size": 0,
    "printString": "11"
}
```