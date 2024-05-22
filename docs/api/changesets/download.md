# Convert chunks to JSON changes

This is a handy service to transform a given set of changes in JSON format to the specific chunk format of the dialect at hand.

Note: the use of `/download` in the URI comes from the fact that this endpoint is used to download changes from Webside.

**URL**: `/changesets/download`

**Method**: `POST`

**Body**: `[change]`

Where change corresponds to the ones defined [here](../changes/post.md)

## Success Responses

**Code** : `200 OK`

**Content**: `string` corresponding to the contents of a chunk formatted file.

**Example:**: changes to Bee chunk file.

```json
[
	{
		"type": "AddClass",
		"label": "'MyPoint'",
		"package": "Examples",
		"timestamp": "2022-08-05T09:27:02.398-03:00",
		"author": "guille",
		"className": "MyPoint",
		"definition": "Point subclass: 'MyPoint' instanceVariableNames: '' classVariableNames: '' poolDictionaries: ''"
	},
	{
		"type": "AddMethod",
		"label": "MyPoint ≫ xTimesY",
		"package": "Examples",
		"timestamp": "2022-08-05T09:27:23.414-03:00",
		"author": "guille",
		"sourceCode": "xTimesY\r\t^x * y",
		"className": "MyPoint",
		"selector": "xTimesY",
		"category": "unclassified"
	}
]
```

**Response body** : `200 OK`

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
