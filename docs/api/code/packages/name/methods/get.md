# Retrieve a package

Retrieve extended methods of a given package.

**URL**: `/packages/{name}/methods`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[method]` as defined in (../../../methods/get.md):

**Example:**: `Webside` package `GET /packages/AST-Core/methods`.

```json
[
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 70,
        "printString": "CompiledMethod>>#firstComment",
        "selector": "firstComment",
        "methodClass": "CompiledMethod",
        "category": "*AST-Core",
        "package": "AST-Core",
        "source": "firstComment\r\t\"Answer a string representing the first comment in the method associated with selector. Return an empty string if the method's source code does not contain a comment.\"\r\r\t^ self comments ifEmpty: [ #() ] ifNotEmpty: [ :comments | comments first ]",
        "author": "",
        "timestamp": "",
        "overriding": false,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 40,
        "printString": "CompiledMethod>>#ast",
        "selector": "ast",
        "methodClass": "CompiledMethod",
        "category": "*AST-Core",
        "package": "AST-Core",
        "source": "ast\r\t\"return an AST for this method. The AST is cached. see class comment of ASTCache\"\r\t^ ASTCache at: self",
        "author": "",
        "timestamp": "",
        "overriding": true,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 68,
        "printString": "CompiledMethod>>#comments",
        "selector": "comments",
        "methodClass": "CompiledMethod",
        "category": "*AST-Core",
        "package": "AST-Core",
        "source": "comments\r\t\"Answer a collection of strings representing the comments in the method. Return an empty collection if the method's source code does not contain a comment.\"\r\r\t^ self ast allComments collect: [:c| c contents]",
        "author": "",
        "timestamp": "",
        "overriding": false,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 123,
        "printString": "CompiledMethod>>#parseTree",
        "selector": "parseTree",
        "methodClass": "CompiledMethod",
        "category": "*AST-Core",
        "package": "AST-Core",
        "source": "parseTree\r\t\"returns an AST for this method, do not cache it. (see #ast for the cached alternative)\"\r\t| ast |\r\tast := self methodClass compiler \r\t\tsource: self sourceCode;\r\t\tfailBlock: [^ self decompile ];\r\t\tparse.\r\tast compilationContext compiledMethod: self.\r\t^ast",
        "author": "",
        "timestamp": "",
        "overriding": false,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 49,
        "printString": "Behavior>>#parseTreeFor:",
        "selector": "parseTreeFor:",
        "methodClass": "Behavior",
        "category": "*AST-Core",
        "package": "AST-Core",
        "source": "parseTreeFor: aSymbol\t\r\t^ (self compiledMethodAt: aSymbol) parseTree",
        "author": "",
        "timestamp": "",
        "overriding": false,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 48,
        "printString": "String>>#isValidSelector",
        "selector": "isValidSelector",
        "methodClass": "String",
        "category": "*AST-Core",
        "package": "AST-Core",
        "source": "isValidSelector\r\t\"check I could be a valid selector (name of method). \r\t For checking if there is symbol like me used as a selector, see #isSelectorSymbol on Symbol\"\r\t^ RBScanner isSelector: self",
        "author": "",
        "timestamp": "",
        "overriding": false,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 48,
        "printString": "String>>#asNumber",
        "selector": "asNumber",
        "methodClass": "String",
        "category": "*AST-Core-Parser",
        "package": "AST-Core",
        "source": "asNumber \r\t\"Answer the Number created by interpreting the receiver as the string \r\trepresentation of a number.\"\r\r\t^Number readFromString: self",
        "author": "",
        "timestamp": "",
        "overriding": false,
        "overriden": false
    },
    {
        "class": "CompiledMethod",
        "hasNamedSlots": true,
        "hasIndexedSlots": true,
        "size": 57,
        "printString": "ScaledDecimal class>>#readFrom:",
        "selector": "readFrom:",
        "methodClass": "ScaledDecimal class",
        "category": "*AST-Core-Parser",
        "package": "AST-Core",
        "source": "readFrom: stringOrStream \r\t\"Answer a decimal number as described on stringOrStream.\r\tThe number may not include a leading radix specification, as in 16rFADE,\r\tnor an exponent like 1.0e-3\r\tIt might have a scale specification at end or not like 10.3s2\r\tIf not, number of digits after decimal point will be used as scale\"\r\t\r\t^(NumberParser on: stringOrStream) nextScaledDecimal",
        "author": "",
        "timestamp": "",
        "overriding": true,
        "overriden": false
    }
]
```
