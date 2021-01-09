# Retrieve evaluation
Retrive the active evaluation with a given ID.

**URL**: `/evaluations/{id}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `evaluation` defined as:
```json
{
    "id": "string",
    "expression": "string"
}
```

**Example:**: `GET /evaluations`
```json
{
    "id": "{207CDBB1-6311-4503-A066-1A89B39A1465}",
    "expression": "1000000 factorial"
}
```