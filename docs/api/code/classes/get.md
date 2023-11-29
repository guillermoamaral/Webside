# Retrieve classes

Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used).
It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy.

**URL**: `/classes`

**Method**: `GET`

**Query Options**

| Option |  Type   | Description                                        |
| :----: | :-----: | -------------------------------------------------- |
|  root  | string  | the name of the root class                         |
| names  | boolean | true to get only class names                       |
|  tree  | boolean | true to get a tree-like structure                  |
| depth  | number  | to limit the hierarchy depth (only when tree=true) |

## Success Responses

**Code** : `200 OK`

**Content**: `[class]` where `class` is defined as:

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

**Example 1:**: `Integer` subclasses `GET /classes?root=Integer`.

```json
[
	{
		"name": "LargeInteger",
		"definition": "Integer variableByteSubclass: #LargeInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "Integer",
		"comment": "I represent integers of more than 30 bits.  These values are beyond the range of SmallInteger, and are encoded here as an array of 8-bit digits. "
	},
	{
		"name": "SmallInteger",
		"definition": "Integer immediateSubclass: #SmallInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "Integer",
		"comment": "My instances are 31-bit numbers or 63-bit numbers depending on the image architecture, stored in twos complement form. The allowable range is approximately +- 1 billion (31 bits), 1 quintillion (63 bits)  (see SmallInteger minVal, maxVal)."
	},
	{
		"name": "LargeNegativeInteger",
		"definition": "LargeInteger variableByteSubclass: #LargeNegativeInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "LargeInteger",
		"comment": "Just like LargePositiveInteger, but represents a negative number."
	},
	{
		"name": "LargePositiveInteger",
		"definition": "LargeInteger variableByteSubclass: #LargePositiveInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "LargeInteger",
		"comment": "I represent positive integers of more than 30 bits (ie, >= 1073741824).  These values are beyond the range of SmallInteger, and are encoded here as an array of 8-bit digits.  Care must be taken, when new values are computed, that any result that COULD BE a SmallInteger IS a SmallInteger (see normalize).\r\rNote that the bit manipulation primitives, bitAnd:, bitShift:, etc., = and ~= run without failure (and therefore fast) if the value fits in 32 bits.  This is a great help to the simulator."
	},
	{
		"name": "Integer",
		"definition": "Number subclass: #Integer\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "Number",
		"comment": "I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger.\r\t\rInteger division consists of:\r\t/\texact division, answers a fraction if result is not a whole integer\r\t//\tanswers an Integer, rounded towards negative infinity\r\t\\\\\tis modulo rounded towards negative infinity\r\tquo: truncated division, rounded towards zero"
	}
]
```

**Example 2:**: `Number` hierarchy in the form of tree `GET /classes?root=Number&tree=true`.

```json
[
    {
        "name": "Number",
        "definition": "Magnitude\r\tsubclass: #Number\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
        "superclass": "Magnitude",
        "comment": "",
        "variable": false,
        "package": "SKernel",
        "subclasses": [
            {
                "name": "Float",
                "definition": "Number\r\tvariableByteSubclass: #Float\r\tclassVariableNames: 'E Infinity MinusInfinity Pi RadiansPerDegree Status'\r\tpoolDictionaries: ''",
                "superclass": "Number",
                "comment": "",
                "variable": true,
                "package": "SKernel",
                "subclasses": []
            },
            {
                "name": "Fraction",
                "definition": "Number\r\tsubclass: #Fraction\r\tinstanceVariableNames: 'numerator denominator'\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
                "superclass": "Number",
                "comment": "",
                "variable": false,
                "package": "SKernel",
                "subclasses": [
                    {
                        "name": "ScaledDecimal",
                        "definition": "Fraction\r\tsubclass: #ScaledDecimal\r\tinstanceVariableNames: 'scale'\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
                        "superclass": "Fraction",
                        "comment": "ScaledDecimal implement a special kind of Fraction that prints in decimal notation.\rIt uses a limited number of digits (scale) after the decimal separation dot and round the result.\rNote that a ScaledDecimal does not printOn: exactly, however it will storeOn: exactly because the full precision fraction is kept in memory.\r\rThis is mostly usefull with denominators being powers of 10.",
                        "variable": false,
                        "package": "SKernel",
                        "subclasses": []
                    }
                ]
            },
            {
                "name": "Integer",
                "definition": "Number\r\tsubclass: #Integer\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
                "superclass": "Number",
                "comment": "gcd: anInteger \r\t\"See Knuth, Vol 2, 4.5.2, Algorithm L\"\r\t\"Initialize\"\r\t| higher u v k uHat vHat a b c d vPrime vPrimePrime q t |\r\thigher := SmallInteger maxVal highBit.\r\tu := self abs max: (v := anInteger abs).\r\tv := self abs min: v.\r\t[v class == SmallInteger]\r\t\twhileFalse: [(uHat := u bitShift: (k := higher - u highBit)) class == SmallInteger\r\t\t\t\tifFalse: [k := k - 1.\r\t\t\t\t\tuHat := uHat bitShift: -1].\r\t\t\tvHat := v bitShift: k.\r\t\t\ta := 1.\r\t\t\tb := 0.\r\t\t\tc := 0.\r\t\t\td := 1.\r\t\t\t\"Test quotient\"\r\t\t\t[(vPrime := vHat + d) ~= 0\r\t\t\t\tand: [(vPrimePrime := vHat + c) ~= 0\r\t\t\t\t\t\tand: [(q := uHat + a // vPrimePrime) = (uHat + b // vPrime)]]]\r\t\t\t\twhileTrue: [\"Emulate Euclid\"\r\t\t\t\t\tc := a - (q * (a := c)).\r\t\t\t\t\t\"Emulate Euclid\"\r\t\t\t\t\td := b - (q * (b := d)).\r\t\t\t\t\tvHat := uHat - (q * (uHat := vHat))].\r\t\t\t\"Multiprecision step\"\r\t\t\tb = 0\r\t\t\t\tifTrue: [v := u rem: (u := v)]\r\t\t\t\tifFalse: [t := u * a + (v * b).\r\t\t\t\t\tv := u * c + (v * d).\r\t\t\t\t\tu := t]].\r\t^ v gcd: u",
                "variable": false,
                "package": "SKernel",
                "subclasses": [
                    {
                        "name": "LargeInteger",
                        "definition": "Integer\r\tvariableByteSubclass: #LargeInteger\r\tclassVariableNames: 'Base Bits DigitLength'\r\tpoolDictionaries: ''",
                        "superclass": "Integer",
                        "comment": "",
                        "variable": true,
                        "package": "SKernel",
                        "subclasses": [
                            {
                                "name": "LargeNegativeInteger",
                                "definition": "LargeInteger\r\tvariableByteSubclass: #LargeNegativeInteger\r\tclassVariableNames: 'LeftLimit'\r\tpoolDictionaries: ''",
                                "superclass": "LargeInteger",
                                "comment": "",
                                "variable": true,
                                "package": "SKernel",
                                "subclasses": []
                            },
                            {
                                "name": "LargePositiveInteger",
                                "definition": "LargeInteger\r\tvariableByteSubclass: #LargePositiveInteger\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
                                "superclass": "LargeInteger",
                                "comment": "(Base squared * 5 + Base - 1) asInteger2 quoRem2: Base.\r(Base squared * 5 + Base - 1) asInteger2 quoRem: Base.\r",
                                "variable": true,
                                "package": "SKernel",
                                "subclasses": []
                            }
                        ]
                    },
                    {
                        "name": "SmallInteger",
                        "definition": "Integer\r\tsubclass: #SmallInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: 'Maximum Minimum'\r\tpoolDictionaries: ''",
                        "superclass": "Integer",
                        "comment": "",
                        "variable": false,
                        "package": "SKernel",
                        "subclasses": []
                    }
                ]
            }
        ]
    }
]
```