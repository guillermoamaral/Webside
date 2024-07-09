# Retrieve object slots

Retrieve the object reached thru the given URI.

This This URI starts with a pinned object ID and contains a _variable_ part, `/*`, that indicates the path from the root (pinned) object to the slot of interest, thru a chain of slots.

Slots might be named or indexed depending on the object class. In the former case, they will correspond to instance variable names while in the latter they will correspond to the indexes between `1` and the size of the object. In any case, they should be known before requesting a given slot. See examples below.

It should be also possible to get the list of both named or indexed slots at once by means `named-slots` or `indexed-slots`, respectively. See examples below.

Finally, custom object views could be available from `custom-views` segment. See [Custom Views](#custom-views) below.

**URL**: `/objects/{id}/*`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `object` defined as:

```json
{
	"id": "string",
	"class": "string",
	"hasNamedSlots": "boolean",
	"hasIndexedSlots": "boolean",
	"size": "number",
	"printString": "string"
}
```

**Example 1:**: `corner` slot of the rectangle with ID `1`, `GET /objects/1/corner`

```json
{
	"class": "Point",
	"hasNamedSlots": true,
	"hasIndexedSlots": false,
	"size": 0,
	"printString": "2 @ 2"
}
```

**Example 2:**: `x` slot of the `corner` slot of the rectangle with ID `1`, `GET /objects/1/corner/x`

```json
{
	"class": "SmallInteger",
	"hasIndexedSlots": false,
	"size": 0,
	"printString": "2"
}
```

**Example 3:**: `named-slots` of the rectangle with ID `1`, `GET /objects/1/named-slots`

```json
[
	{
		"class": "Point",
		"hasNamedSlots": true,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "1 @ 1",
		"slot": "origin"
	},
	{
		"class": "Point",
		"hasNamedSlots": true,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "2 @ 2",
		"slot": "corner"
	}
]
```

Note the `slot` property in each object.

**Example 4:**: `named-slots` of the `corner` slot of the rectangle with ID `1`, `GET /objects/1/corner/named-slots`

```json
[
	{
		"class": "SmallInteger",
		"hasNamedSlots": false,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "2",
		"slot": "x"
	},
	{
		"class": "SmallInteger",
		"hasNamedSlots": false,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "2",
		"slot": "y"
	}
]
```

**Example 5:**: `indexed-slots` of the array with ID `1`, `GET /objects/1/indexed-slots`

```json
[
	{
		"class": "True",
		"hasNamedSlots": false,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "true",
		"slot": 1
	},
	{
		"class": "SmallInteger",
		"hasNamedSlots": false,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "2",
		"slot": 2
	},
	{
		"class": "UndefinedObject",
		"hasNamedSlots": false,
		"hasIndexedSlots": false,
		"size": 0,
		"printString": "nil",
		"slot": 3
	}
]
```

**Example 6:**: element at index `2` of the same array as before, `GET /objects/1/2`

```json
{
	"class": "SmallInteger",
	"hasNamedSlots": false,
	"hasIndexedSlots": false,
	"size": 0,
	"printString": "2"
}
```

# Custom views

Custom views give the chance to specify special ways of viewing an object. The consumer of the API can take advantage of these specifications and display different views accordingly.\
For example, at the moment of writing this documentation, the IDE supports the following views `list`, `table`, `tree`, `source`, `html` and `markdown`. These views are available in the Inspector.

Custom views should be accessible through `/custom-views` segment.

For example, lets suppose there is a instance of a tree-like object in your system pinned with the ID 3.
Responding to `GET /objects/2/custom-views` with the following specification will enable a tree titled `"Tree View"`.

```json
[
	{
		"type": "tree",
		"title": "Tree View",
		"roots": [
			{
				"label": "root",
				"children": [
					{
						"label": "child 1"
					},
					{
						"label": "child 2",
						"children": [
							{
								"label": "subchild 3"
							}
						]
					}
				]
			}
		]
	}
]
```

The JSON structure for this type of view should be a list of `node`, where a `node` should have this properties:

```json
{
	"label": "string",
	"children": ["node"]
}
```

It is possible to use other properties than `label` and `children` (default ones), provided they are specified in `nodeLabel` and `nodeChildren`, respectively.\
For instance, the same example but with custom properties `name` and `decendents`:

```json
[
	{
		"type": "tree",
		"title": "Tree View",
		"label": "name",
		"children": "decendents",
		"roots": [
			{
				"name": "root",
				"decendents": [
					{
						"name": "child 1"
					},
					{
						"name": "child 2",
						"decendents": [
							{
								"name": "subchild 3"
							}
						]
					}
				]
			}
		]
	}
]
```

Here is another example using the type `html`. This type is the most flexible as an object can specify any arbitrary HTML code.

```json
[
	{
		"type": "html",
		"title": "My Fancy View",
		"code": "<!DOCTYPE html>\r<html lang=\"en\">\r<head>\r  <meta charset=\"UTF-8\">\r  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">\r  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\r  <style>\r\r* {\r  padding: 0;\r  margin: 0 ;\r  box-sizing: border: box;\r}\r\rbody {\r  background-image: linear-gradient(to right, #f78ca0 0%, #f9748f 19%, #fd868c 60%, #fe9a8b 100%);\r  background-attachment: fixed;\r  height: 100vh;\r  display: grid;\r  justify-content: center;\r  align-content: center;\r  grid-template-columns: minmax(150px, 440px);\r  font-family: 'Montserrat', sans-serif;\r}\r\r#wrapper {\r  max-width: 400px;\r  padding: 20px;\r}\r\r#title {\r  margin-bottom: 20px;\r}\r\rblockquote {\r  border-left: 5px solid white;\r  padding-left: 20px;\r}\r</style>\r  <title>Blockquote</title>\r</head>\r<body>\r  <div id=\"wrapper\">\r    <h1 id=\"title\">Fancy Object</h1>\r    <blockquote>\r      This is my facy view.\r    </blockquote>\r  </div>\r</body>\r</html>"
	}
]
```
