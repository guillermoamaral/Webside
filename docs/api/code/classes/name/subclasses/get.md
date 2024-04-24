# Retrieve categories

Retrieve subclasses of a given class.

**URL**: `/class/{name}/subclasses`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[string]`

**Example:**: `Integer` subclasses `GET /classes/Integer/subclasses`.

```json
[
    {
        "name": "LargeInteger",
        "definition": "Integer variableByteSubclass: #LargeInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
        "superclass": "Integer",
        "comment": "I represent integers of more than 30 bits.  These values are beyond the range of SmallInteger, and are encoded here as an array of 8-bit digits. ",
        "category": "Numbers",
        "variable": true,
        "package": "Kernel"
    },
    {
        "name": "SmallInteger",
        "definition": "Integer immediateSubclass: #SmallInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
        "superclass": "Integer",
        "comment": "My instances are 31-bit numbers or 63-bit numbers depending on the image architecture, stored in twos complement form. The allowable range is approximately +- 1 billion (31 bits), 1 quintillion (63 bits)  (see SmallInteger minVal, maxVal).\r\rHandy guide to the kinds of Integer division:\r- /  exact division, returns a fraction if result is not a whole integer.\r- //  returns an Integer, rounded towards negative infinity.\r- \\\\ is modulo rounded towards negative infinity.\r- quo:  truncated division, rounded towards zero.",
        "category": "Numbers",
        "variable": false,
        "package": "Kernel"
    }
]
```
