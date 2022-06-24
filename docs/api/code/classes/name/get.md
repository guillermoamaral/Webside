# Retrieve a class

Retrieve the class with a given name.

**URL**: `/classes/{name}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: a `class` defined as:

```json
{
	"name": "string",
	"definition": "string",
	"superclass": "string",
	"comment": "string",
	"variable": "boolean",
	"package": "string"
}
```

**Example:**: `Integer` class `GET /classes/Integer`.

```json
{
	"name": "Integer",
	"definition": "Number subclass: #Integer\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
	"superclass": "Number",
	"comment": "I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger.\r\t\rInteger division consists of:\r\t/\texact division, answers a fraction if result is not a whole integer\r\t//\tanswers an Integer, rounded towards negative infinity\r\t\\\\\tis modulo rounded towards negative infinity\r\tquo: truncated division, rounded towards zero"
}
```
