# Retrieve object slots
Retrive slots of pinned object or one of its descendants.

This endpoint contains a _variable_ part, `/*`, in the URI that serves to indicate the path from the root object to the slot of interest by naming each slot in between.

Slots might be named or indexed depending on the object class. In the former case they will correspond to instance variable names while in the latter they will correspond to the indexes between 1 and the object size. In any case, they should be known before requesting a given slot.

Webside will distinguish one case from the other based on the `indexable` property (common to every object) and will use `size` property in case of an indexed object.

To get named slots, Webside (and the inspector in particular), will make a request to retrieve instance variable names of the object class ([/classes/{name}/instance-variables](../../../classes/instance-variables/get.md)).
In case of indexable objects, it will use slots `1` to `n`, with `n` corrsponding to the object size.

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

**Example:**: `x` slot of `corner` slot of the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/corner/x`
```json
{
    "class": "SmallInteger",
    "indexable": false,
    "size": 0,
    "printString": "11"
}
```