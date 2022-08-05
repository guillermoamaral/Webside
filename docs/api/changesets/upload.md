# JSON changes to chunk-formatted file

This is a handy service to transform a file in chunk format specific of the dialect at hand to a set of changes in JSON.

Note: the use of `/upload` in the URI comes from the fact that this endpoint is used to upload changes to Webside.

**URL**: `/changesets/upload`

**Method**: `POST`

**Body**: `string` corresponding to the contents of a chunk formatted file.

## Success Responses

**Code** : `200 OK`

**Content**: `[change]`

Where change corresponds to the ones defined [here](../changes/post.md)

**Example:**:

```
!ClassDefinition
timestamp: '2022-08-05T09:27:46.698-03:00'
author: 'guille'
className: 'MyPoint'
project: 'Examples'!
Point subclass: 'MyPoint' instanceVariableNames: '' classVariableNames: '' poolDictionaries: ''!

!MethodDefinition
timestamp: '2022-08-05T09:27:46.701-03:00'
author: 'guille'
className: 'MyPoint'
selector: #xTimesY
category: #unclassified
project: 'Examples'!
xTimesY
^x \* y!
```

**Response body** : `200 OK`

```json
[
	{
		"type": "AddClass",
		"label": "'MyPoint'",
		"package": "Examples",
		"timestamp": "2022-08-05T09:27:02.398-03:00",
		"author": "guille",
		"class": "MyPoint",
		"definition": "Point subclass: 'MyPoint' instanceVariableNames: '' classVariableNames: '' poolDictionaries: ''"
	},
	{
		"type": "AddMethod",
		"label": "MyPoint ≫ xTimesY",
		"package": "Examples",
		"timestamp": "2022-08-05T09:27:23.414-03:00",
		"author": "guille",
		"sourceCode": "xTimesY\r\t^x * y",
		"class": "MyPoint",
		"selector": "xTimesY",
		"category": "unclassified"
	}
]
```
