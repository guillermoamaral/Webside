# Retrieve instance variables
Retrieve both instance and class variables of a given class. This endpoint is the equivalent of retrieving instance and class variables using the corresponding endpoints.

**URL**: `/class/{name}/variables`

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

**Example:**: `Fraction` instance variables `GET /classes/Fraction/variables`.
```json
[
    {
        "name": "numerator",
        "class": "Fraction",
        "type": "instance"
    },
    {
        "name": "denominator",
        "class": "Fraction",
        "type": "instance"
    },
    {
        "name": "DependentsFields",
        "class": "Object class",
        "type": "class"
    }
]
```