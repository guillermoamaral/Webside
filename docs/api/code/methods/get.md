# Retrieve methods
Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided).    

**URL**: `/methods`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| ------------- | ------------- | ------------- |
| selector | string | to get implementors of such selector | 
| sending | string | to get senders of such selector |
| class | string | to get methods implemented by a given class |
| referencingClass | string | to get those methods referencing a given class |
| cagegory | string | to get methods under a given category |
| referencingVariable | string | to get those methods referencing (using or assigning) a given variable |
| usingVariable | string | to get those methods using a given variable |
| assigningVariable | string | to get those methods assigning a given variable |

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
_Note: common properties of every `object` such as `class` are also included though they are not listed here._

**Example:**: `Fraction` methods under `arithmetic` category and sending `reciprocal` `GET /methods?class=Fraction&category=arithmetic&sending=reciprocal`..
```json
[
    {
        "selector": "raisedToInteger:",
        "class": "Fraction",
        "category": "arithmetic",
        "source": "raisedToInteger: anInteger\r\t| num den |\r\tanInteger = 0 ifTrue: [\r\t\t^self isZero\r\t\t\tifTrue: [(ArithmeticError on: #raisedToInteger:) signalInvalidOperation]\r\t\t\tifFalse: [1]].\r\tanInteger < 0 ifTrue: [^self reciprocal raisedToInteger: anInteger negated].\r\tnum := numerator raisedToInteger: anInteger.\r\tden := denominator raisedToInteger: anInteger.\r\t^self class numerator: num denominator: den",
        "author": "John Doe",
        "timestamp": "2018-11-14T15:58:20.238-03:00",
        "project": "Kernel"
    },
    {
        "selector": "/",
        "class": "Fraction",
        "category": "arithmetic",
        "source": "/ aNumber\r\t^self * aNumber reciprocal",
        "author": "John Doe",
        "timestamp": "2018-11-06T15:52:59.864-03:00",
        "project": "Kernel"
    }
]
```