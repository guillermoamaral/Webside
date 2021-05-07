# Retrieve methods
Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided).    

**URL**: `/methods`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| -- | -- | -- |
| selector | string | to get implementors of a given selector | 
| sending | string | to get senders of a given selector |
| class | string | to get methods implemented by a given class |
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
    "project": "string",
    "overriding": "boolean",
	"overriden": "boolean"
}
```

**Example:**: `Fraction` methods under `arithmetic` category and sending `reciprocal` `GET /methods?class=Fraction&category=arithmetic&sending=reciprocal`.
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