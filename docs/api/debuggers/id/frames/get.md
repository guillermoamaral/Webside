# Retrieve object slots
Retrieve frames of the debugger with a given ID.

**URL**: `/debuggers/{id}/frames`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[frame]` where `frame` is defined as:
```json
[
{
    "index": "number",
    "label": "string",
    "class": "class",
    "method": "method",
    "interval": "interval"
}
]
```

Where [`class`](../../../../../code/classes/name/get.md) the class of the receiver, [`method`](../../../../../objects/id/get.md) is the method associated to the frame,  and `interval` contains the starting and ending positions of the current AST node within the source code. 

```json
{
    "start": "number",
    "end": "number"
}
```


**Example:**: frames of debugger with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /debugger/{207CDBB1-6311-4503-A066-1A89B39A1465}/frames`
```json
[
    
]
```