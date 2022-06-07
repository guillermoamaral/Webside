# Retrieve methods

Retrieve those methods satisfying the condition specified in the filtering query options. In case no condition is specified, all the methods in the system should be retrieved.\
Of course, the combination of filtering options should be interpreted as the intersection of methods honoring each option. For instance, if the query is `class=Rectangle&category=testing&accessing=origin&sending=<` then every method of `Rectangle`, in category `testing`, accessing the instance variable `origing`, and sending `<` should be retrieved (`Rectangle >> #intersects:` honors such condition in Bee, Pharo, Squeak and Dolphin at the moment of writing this documentation).\

The `scope` option should be used in conjunction with other options to restrict the scope of a given search. For instance, `selector=<&scope=Magnitude` should retrieve implementors of `<` in `Magnitude` hierarchy (including superclasses).\
Note that this could be other than a class (a package for instance). It will depend on the implementation of the API.

**URL**: `/methods`

**Method**: `GET`

**Filtering Query Options**
| Option | Type | Description |
| -- | -- | -- |
| class | string | to get methods of a given class |
| selector | string | to get implementors of a given selector |
| category | string | to get methods under a given category |
| accessing | string | to get those methods accessing a given variable (either using or assigning it) |
| using | string | to get those methods using a given variable |
| assigning | string | to get those methods assigning a given variable |
| sending | string | to get senders of a given selector |
| referencingClass | string | to get those methods referencing a given class |
| scope | string | used to restrict the search to a given scope (only class at the moment of this writing) |

**Decoration Query Options**
| Option | Type | Description |
| -- | -- | -- |
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
	"disassembly": "string",
	"ast": "node"
}
```

_Note: optional properties such as `bytecodes` or `disassembly` should not be included if they are not requested in the query._

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

The AST of a method could be obtained by specifying the option `ast=true`. Though this option is not mandatory, Webside will take advantage of it when it is available to, for instance, detect the selector under the cursor (if any), and provide better senders/implementors facilities (think of a keyword selector).
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
