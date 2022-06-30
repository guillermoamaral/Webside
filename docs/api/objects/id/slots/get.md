# Retrieve object slots

Retrive the object reached thru the given URI.

This This URI starts with a pinned object ID and contains a _variable_ part, `/*`, that indicates the path from the root (pinned) object to the slot of interest, thru a chain of slots.

Slots might be named or indexed depending on the object class. In the former case, they will correspond to instance variable names while in the latter they will correspond to the indexes between `1` and the object size. In any case, they should be known before requesting a given slot. See examples below.

It should be also possible to get the list of both named or indexed slots by means of a last segment `named-slots` or `indexed-slots`, respectively. See examples below.

**URL**: `/objects/{id}/*`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `object` defined as:

```json
{
	"id": "string",
	"class": "string",
	"indexable": "boolean",
	"size": "number",
	"printString": "string"
}
```

**Example 1:**: `corner` slot of the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/corner`

```json
{
	"class": "Point",
	"indexable": false,
	"size": 0,
	"printString": "2 @ 2"
}
```

**Example 2:**: `x` slot of the `corner` slot of the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/corner/x`

```json
{
	"class": "SmallInteger",
	"indexable": false,
	"size": 0,
	"printString": "2"
}
```

**Example 3:**: `named-slots` of the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/named-slots`

```json
[
	{
		"class": "Point",
		"indexable": false,
		"size": 0,
		"printString": "1 @ 1",
		"slot": "origin"
	},
	{
		"class": "Point",
		"indexable": false,
		"size": 0,
		"printString": "2 @ 2",
		"slot": "corner"
	}
]
```

Note the `slot` property in each object.

**Example 4:**: `named-slots` of the `corner` slot of the rectangle with ID `{207CDBB1-6311-4503-A066-1A89B39A1465}`, `GET /objects/{207CDBB1-6311-4503-A066-1A89B39A1465}/corner/named-slots`

```json
[
	{
		"class": "SmallInteger",
		"indexable": false,
		"size": 0,
		"printString": "2",
		"slot": "x"
	},
	{
		"class": "SmallInteger",
		"indexable": false,
		"size": 0,
		"printString": "2",
		"slot": "y"
	}
]
```

**Example 5:**: `indexed-slots` of the array with ID `{B53F7681-E7CF-4E92-B260-BC33194D2EBB}`, `GET /objects/{B53F7681-E7CF-4E92-B260-BC33194D2EBB}/indexed-slots`

```json
[
	{
		"class": "True",
		"indexable": false,
		"size": 0,
		"printString": "true",
		"slot": 1
	},
	{
		"class": "SmallInteger",
		"indexable": false,
		"size": 0,
		"printString": "2",
		"slot": 2
	},
	{
		"class": "UndefinedObject",
		"indexable": false,
		"size": 0,
		"printString": "nil",
		"slot": 3
	}
]
```

**Example 6:**: element at index `2` of the same array as before, `GET /objects/{B53F7681-E7CF-4E92-B260-BC33194D2EBB}/2`

```json
{
	"class": "SmallInteger",
	"indexable": false,
	"size": 0,
	"printString": "2"
}
```
