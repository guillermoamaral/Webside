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
As metioned above, the AST of a method could be obtained by specifying the option `ast=true`. Though it is not mandatory that the API implements this option, Webside will take advantage if it is available. For instance, to detect the selector under the cursor, if any, and provide better senders/implementors facilities (think of a keyword selector).
In case the option is provided by the backend Smalltalk, the expected structure of the `ast` should have the following `node` basic structure:

```json
{
    "type": "string",
    "start": "number",
    "end": "number",
    "children": ["node"]
}
```
Where `start` and `end` represent the span of the node over the source code.
Also, some leaf nodes should contain a `value` property with their corresponding value.
The following table lists possible types properties whenever it applies.

**AST node types and properties**
| Type | Additional properties |
| -- | -- |
| Method | - |
| Comment | value (string) |
| Message | - |
| Selector | value (string) |
| Identifier | value (string) |
| Literal | value (string) |

Note that this structure corresponds to a rather simplified AST, which might be richer in some implementations. This is due to the _unification_ spirit of Webside, conceived to support different Smmaltalk dialects.