# Retrieve classes
Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used).
It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy.

**URL**: `/classes`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| ------------- | ------------- | ------------- |
| root | string | the name of the root class |
| names | boolean | true to get only project names |
| tree | boolean | true to get a tree-like structure |
| depth | number | to limit the hierarchy depth (only when tree=true) |

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
    "project": "string"
}
```

**Example:**: `Number` subclasses `GET /classes?root=Number`.
```json
[
    {
        "name": "Number",
        "definition": "Magnitude\r\tsubclass: #Number\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
        "superclass": "Magnitude",
        "comment": "",
        "variable": false,
        "project": "SKernel"
    },
    {
        "name": "Float",
        "definition": "Number\r\tvariableByteSubclass: #Float\r\tclassVariableNames: 'E Infinity MinusInfinity Pi RadiansPerDegree Status'\r\tpoolDictionaries: ''",
        "superclass": "Number",
        "comment": "",
        "variable": true,
        "project": "SKernel"
    },
    {
        "name": "Fraction",
        "definition": "Number\r\tsubclass: #Fraction\r\tinstanceVariableNames: 'numerator denominator'\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
        "superclass": "Number",
        "comment": "",
        "variable": false,
        "project": "SKernel"
    },
    {
        "name": "ScaledDecimal",
        "definition": "Fraction\r\tsubclass: #ScaledDecimal\r\tinstanceVariableNames: 'scale'\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
        "superclass": "Fraction",
        "comment": "ScaledDecimal implement a special kind of Fraction that prints in decimal notation.\rIt uses a limited number of digits (scale) after the decimal separation dot and round the result.\rNote that a ScaledDecimal does not printOn: exactly, however it will storeOn: exactly because the full precision fraction is kept in memory.\r\rThis is mostly usefull with denominators being powers of 10.",
        "variable": false,
        "project": "SKernel"
    },
    {
        "name": "Integer",
        "definition": "Number\r\tsubclass: #Integer\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
        "superclass": "Number",
        "comment": "gcd: anInteger \r\t\"See Knuth, Vol 2, 4.5.2, Algorithm L\"\r\t\"Initialize\"\r\t| higher u v k uHat vHat a b c d vPrime vPrimePrime q t |\r\thigher := SmallInteger maxVal highBit.\r\tu := self abs max: (v := anInteger abs).\r\tv := self abs min: v.\r\t[v class == SmallInteger]\r\t\twhileFalse: [(uHat := u bitShift: (k := higher - u highBit)) class == SmallInteger\r\t\t\t\tifFalse: [k := k - 1.\r\t\t\t\t\tuHat := uHat bitShift: -1].\r\t\t\tvHat := v bitShift: k.\r\t\t\ta := 1.\r\t\t\tb := 0.\r\t\t\tc := 0.\r\t\t\td := 1.\r\t\t\t\"Test quotient\"\r\t\t\t[(vPrime := vHat + d) ~= 0\r\t\t\t\tand: [(vPrimePrime := vHat + c) ~= 0\r\t\t\t\t\t\tand: [(q := uHat + a // vPrimePrime) = (uHat + b // vPrime)]]]\r\t\t\t\twhileTrue: [\"Emulate Euclid\"\r\t\t\t\t\tc := a - (q * (a := c)).\r\t\t\t\t\t\"Emulate Euclid\"\r\t\t\t\t\td := b - (q * (b := d)).\r\t\t\t\t\tvHat := uHat - (q * (uHat := vHat))].\r\t\t\t\"Multiprecision step\"\r\t\t\tb = 0\r\t\t\t\tifTrue: [v := u rem: (u := v)]\r\t\t\t\tifFalse: [t := u * a + (v * b).\r\t\t\t\t\tv := u * c + (v * d).\r\t\t\t\t\tu := t]].\r\t^ v gcd: u",
        "variable": false,
        "project": "SKernel"
    },
    {
        "name": "LargeInteger",
        "definition": "Integer\r\tvariableByteSubclass: #LargeInteger\r\tclassVariableNames: 'Base Bits DigitLength'\r\tpoolDictionaries: ''",
        "superclass": "Integer",
        "comment": "",
        "variable": true,
        "project": "SKernel"
    },
    {
        "name": "LargeNegativeInteger",
        "definition": "LargeInteger\r\tvariableByteSubclass: #LargeNegativeInteger\r\tclassVariableNames: 'LeftLimit'\r\tpoolDictionaries: ''",
        "superclass": "LargeInteger",
        "comment": "",
        "variable": true,
        "project": "SKernel"
    },
    {
        "name": "LargePositiveInteger",
        "definition": "LargeInteger\r\tvariableByteSubclass: #LargePositiveInteger\r\tclassVariableNames: ''\r\tpoolDictionaries: ''",
        "superclass": "LargeInteger",
        "comment": "(Base squared * 5 + Base - 1) asInteger2 quoRem2: Base.\r(Base squared * 5 + Base - 1) asInteger2 quoRem: Base.\r",
        "variable": true,
        "project": "SKernel"
    },
    {
        "name": "SmallInteger",
        "definition": "Integer\r\tsubclass: #SmallInteger\r\tinstanceVariableNames: ''\r\tclassVariableNames: 'Maximum Minimum'\r\tpoolDictionaries: ''",
        "superclass": "Integer",
        "comment": "",
        "variable": false,
        "project": "SKernel"
    }
]
```