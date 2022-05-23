# Retrieve methods
Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided).    

**URL**: `/methods`

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
| ast | boolean | to get methods' AST (see below) |
| bytecodes | boolean | to get methods' bytecodes |
| disassembly | boolean | to get methods' disassembly |

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
	"overriden": "boolean",
    "bytecodes": "string",
	"disassembly": "string"
}
```

_Note: optional properties such as `bytecodes` or `disassembly` won't be included if they are not requested in the query._ 

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

## Method AST
As metioned above, the AST of methods could be obtained by specifying the option `ast=true`. Though this option is not mandatory, Webside will take advantage of it when available.
In case it is provided, the expected structure of the `ast` property should have following `node` basic structure:

```json
{
    "type": "string",
    "start": "number",
    "end": "number",
    "children": ["node"]
}
```
Where `start` and `end` represent the span of the node over the source code.
Of course each particular type of node will contain specific properties. The following table lists possible types together with their additional properties.

**AST node types and properties**
| Type | Properties |
| -- | -- |
| "Assigment" | string | 