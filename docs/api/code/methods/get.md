# Retrieve methods

Retrieve those methods satisfying the condition specified in the filtering query options. In case no condition is specified, all the methods in the system should be retrieved.\
Of course, the combination of filtering options should be interpreted as the intersection of methods honoring each option. For instance, if the query is `class=Rectangle&category=testing&accessing=origin&sending=<` then every method of `Rectangle`, in category `testing`, accessing the instance variable `origing`, and sending `<` should be retrieved (`Rectangle >> #intersects:` honors such condition in Bee, Pharo, Squeak and Dolphin at the moment of writing this documentation).

The `class`, `hierarchy` and `package` options can be used in conjunction with other options to restrict the scope of a given search. For instance, `selector=<&hierarhcy=Magnitude` should retrieve implementors of `<` in `Magnitude` hierarchy (including superclasses).

**URL**: `/methods`

**Method**: `GET`

**Filtering Query Options**

| Option           |  Type   | Description                                                                    |
| ---------------- | :-----: | ------------------------------------------------------------------------------ |
| class            | string  | to get methods of a given class                                                |
| selector         | string  | to get implementors of a given selector                                        |
| category         | string  | to get methods under a given category                                          |
| accessing        | string  | to get those methods accessing a given variable (either using or assigning it) |
| using            | string  | to get those methods using a given variable                                    |
| assigning        | string  | to get those methods assigning a given variable                                |
| sending          | string  | to get senders of a given selector                                             |
| referencingClass | string  | to get those methods referencing a given class                                 |
| selectorMatching | string  | to get those methods with a selector matching a given pattern string           |
| hierarchy        | string  | used to restrict the search to a given hierarchy                               |
| package          | string  | used to restrict the search to a given package                                 |
| count            | boolean | true to get only the number of methods statisfying the condition               |

**Decoration Query Options**

|   Option    |  Type   | Description                                                    |
| :---------: | :-----: | -------------------------------------------------------------- |
|     ast     | boolean | to get methods' AST ([see below](#method-ast))                 |
| annotations | boolean | to get methods' annotations ([see below](#method-annotations)) |
|  bytecodes  | boolean | to get methods' bytecodes                                      |
| disassembly | boolean | to get methods' disassembly                                    |

## Success Responses

**Code** : `200 OK`

**Content**: `[method]` where `method` is defined as:

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

Where:

-   `needsRecompilation` specifies whether the last compilation of method has any issue, for instance, when a instance variable was removed from the class definition and a method accessing it should be revised and recompiled. This mark is used to color the method differently to bring developers attention.
-   `ast` is a simplified version of the method's abstract syntax tree used for some functionalities (see [Method AST](#method-ast) below)
-   `annotations` is list of annotations helpful to code linting (see [Method Annotations](#method-annotations) below)

_Note: optional properties such as `bytecodes`, `disassembly` or `annotations` should not be included if they are not requested in the query._

**Example:**: `Fraction` methods under `arithmetic` category and sending `reciprocal` `GET /classes/Fraction/methods?category=arithmetic&sending=reciprocal`.

```json
[
	{
		"selector": "/",
		"source": "/ aNumber\r\t\"Answer the result of dividing the receiver by aNumber.\"\r\taNumber isFraction\r\t\tifTrue: [^self * aNumber reciprocal].\r\t^ aNumber adaptToFraction: self andSend: #/",
		"methodClass": "Fraction",
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
	"label": "string",
	"start": "number",
	"end": "number",
	"children": ["node"]
}
```

Where `type` corresponds to the node type, and `start`-`end` represent the span of the node over the source code.
Also, some leaf nodes should contain a `value` property with their corresponding value.
The following table lists possible types properties whenever it applies.

**AST node types and properties**

|    Type    | Additional properties |
| :--------: | --------------------- |
|   Method   | -                     |
|  Comment   | value (string)        |
|  Message   | -                     |
|  Selector  | value (string)        |
| Identifier | value (string)        |
|  Literal   | value (string)        |

Note that `type` property depends on the target system and thus may vary from one dialect to another. However, in order to take advantage of it, it is expected to include these ones `Selector`, `Identifier` and `Literal`.

**Note that this structure corresponds to a rather simplified AST, which might be richer in some implementations. This is due to the _unification_ spirit of Webside, conceived to support different Smmaltalk dialects.**

## Method Annotations

Annotations could be obtained by means of the option `annotations=true`. Webside will take advantage of them to decorate the code editor with annotations in a gutter specially for that.
The structure of an `annotation` should be like this:

```json
{
	"type": "string",
	"start": "number",
	"end": "number",
	"description": "string"
}
```

`type` could be either `"warning"` or `"error"`.
`start` and `end` represent the span within the source code over which the annotation applies.
Finally, `description` is the actual annotation.
Here are a couple of examples of methods with annotations, one sending a message that does not have any implementor, and another with a temporary variable used but not assigned (prior its ussage).

```json
[
	{
		"selector": "m",
		"methodClass": "Number",
		"category": "blah",
		"source": "m\r\t^self messageThatHasNoImplementors",
		"author": "Guille",
		"package": "Blah",
		"annotations": [
			{
				"from": 10,
				"to": 38,
				"type": "warning",
				"description": "messageThatHasNoImplementors has no implementors"
			}
		]
	}
]
```

```json
[
	{
		"selector": "m",
		"methodClass": "Point",
		"category": "blah",
		"source": "m\r\t| t |\r\t^t m",
		"author": "guille",
		"package": "Blah",
		"annotations": [
			{
				"from": 12,
				"to": 13,
				"type": "warning",
				"description": "t is not assigned"
			}
		]
	}
]
```

## Test Methods

Every Smalltalk dialect provides some sort of unit tests (e.g., SUnit). In any case, a test is usually a method that can be _ran_, either isolatedly or in the context of a _test suite_. This implies additional IDE options, and sometimes visual decorations to distinguish them from regular methods.

Webside has a naive, yet effective way to detect whether a method represents a test, so the backend is not obligated to specify that.

There are some dialects that keep track of the last run result. They used it as a test _state_, usually to show a colored icon: green if the last run resulted in the test passing, yellow if it failed, and red if it ended up in an error.\
If the backend at hand provides such information (in a property `status`), Webside will display it as a mark near the selector.

```json
{
	"status": "string"
}
```

Where `status` can be either `"passed"`, `"failed"` or `"error"`.
