# Retrieve extensions

Retrieve IDE extensions.

**URL**: `/extensions`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[definition]` where `definition` like described [here](./README.md).

**Example:** one `change` extension to move a method to the superclass of its current class:

```json
[
	{
		"extensionType": "change",
		"elementType": "method",
		"label": "Move to superclass",
		"properties": {
			"type": "MoveUpMethod",
			"className": "element.methodClass",
			"selector": "element.selector"
		},
		"parameters": []
	}
]
```
