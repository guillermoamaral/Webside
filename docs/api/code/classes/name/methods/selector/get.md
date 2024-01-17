# Retrieve methods
Retrieve the method with the given selector in the given class.

**URL**: `classes/{name}/methods/{selector}`

**Method**: `GET`

**Code** : `200 OK`

**Content**: a `method` defined as:

```json
{
	"selector": "string",
	"methodClass": "string",
	"category": "string",
	"source": "string",
	"author": "string",
	"timestamp": "string",
	"package": "string",
	"needsRecompilation": "boolean",
	"overriding": "boolean",
	"overriden": "boolean",
	"bytecodes": "string",
	"disassembly": "string",
	"ast": "node",
	"annotations": ["annotation"]
}
```
More details in [here](../../methods/get.md).

**Example:**: `isFraction` method in `Fraction`.

```json
{
    "selector": "isFraction",
    "methodClass": "Fraction",
    "category": "converting",
    "package": "Kernel",
    "source": "isFraction\r\t^ true",
    "author": "",
    "timestamp": "",
    "overriding": true,
    "overriden": true
}
```