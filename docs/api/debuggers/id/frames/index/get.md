# Retrieve debugger frame
Retrieve the _i_-th frame withing the debugger with a given ID.

**URL**: `/debuggers/{id}/frames/{index}`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `frame` where `frame` is defined as:
```json
{
    "index": "number",
    "label": "string",
    "class": "class",
    "method": "method",
    "interval": "interval"
}
```

Where `class` is the class of the receiver as defined [here](../../../../code/classes/name/get.md), `method` is the method associated to the frame as defined [here](../../../../code/methods/get.md), and `interval` contains the starting and ending positions of the current AST node within the source code. 

```json
{
    "start": "number",
    "end": "number"
}
```

**Example:**: 2nd frame of debugger with ID `{7B9C472C-376F-493A-9B88-5CCBA70A4309}`, `GET /debugger/{7B9C472C-376F-493A-9B88-5CCBA70A4309}/frames/2`
```json
{
	"label": "SmallInteger(Integer)>>factorial",
    "index": 2,
	"class": {
		"name": "SmallInteger",
		"definition": "Integer\r\tsubclass: #SmallInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: 'Maximum Minimum'\r\tpoolDictionaries: ''",
		"superclass": "Integer",
		"comment": "",
		"variable": false,
		"project": "Kernel"
	},
	"method": {
		"selector": "factorial",
		"class": "Integer",
		"category": "arithmetic",
		"source": "factorial\r\tself > 1 ifTrue: [^(self - 1) factorial * self].\r\tself < 0 ifTrue: [^self error: 'not valid for negative numbers'].\r\t^1",
		"author": "Jon Doe",
		"timestamp": "2018-10-26T12:37:22.459-03:00",
		"project": "Kernel",
		"overriding": false,
		"overriden": false
	},
	"interval": {
		"start": 31,
		"end": 50
	}
}
```