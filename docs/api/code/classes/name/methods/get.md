# Retrieve methods
Retrieve methods of a given class.

**URL**: `/class/{name}/variables`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| -- | -- | -- |
| selector | string | to get implementors of a given selector | 
| sending | string | to get senders of a given selector |
| referencing | string | to get those methods referencing a given class or variable (either using or assigning it) |
| category | string | to get methods under a given category |
| using | string | to get those methods using a given variable |
| assigning | string | to get those methods assigning a given variable |

## Success Responses

**Code** : `200 OK`

**Content**: `[method]` where `method` is defined as:
```json
{
    "selector": "string",
    "class": "string",
    "category": "string",
    "source": "string",
    "author": "string",
    "timestamp": "string",
    "project": "string"
}
```

**Example:**: `Fraction` methods under `arithmetic` category and sending `reciprocal` `GET /classes/Fraction/methods?category=arithmetic&sending=reciprocal`.
```json
[
    {
        "selector": "/",
        "source": "/ aNumber\r\t\"Answer the result of dividing the receiver by aNumber.\"\r\taNumber isFraction\r\t\tifTrue: [^self * aNumber reciprocal].\r\t^ aNumber adaptToFraction: self andSend: #/",
        "class": "Fraction",
        "category": "arithmetic"
    }
]
```