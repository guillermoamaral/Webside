# Compile a method

Compile a method in a given class.

**URL**: `/classes/{name}`

**Method**: `POST`

**Payload**:

```json
{
	"category": "string",
	"package": "string",
	"sourceCode": "string"
}
```

Note that `className` property is not necessary as it is redundant with the class specified in the URL.

## Success Responses

**Code** : `200 OK`

**Example:**: `POST /classes/Point` with the following payload:

```json
{
	"category": "examples",
	"package": "Webside",
	"sourceCode": "xy ^x * y"
}
```

The response should contain the compiled method:

```json
{
	"selector": "xy",
	"methodClass": "Point",
	"category": "examples",
	"source": "xy\r\t^x * y",
	"package": "Webside"
}
```

## Compilation Errors

Compilation errors should be treated as when using changes (see [here](/docs/api/changes/post.md#compilation-errors)), except for the `suggestions`, that imply the use of changes.\
\
For example, suppose the following payload:

```json
{
	"category": "examples",
	"package": "Webside",
	"sourceCode": "xy ^x *"
}
```

The response should look like:

```json
409
{
	"description": "primary missing",
	"fullDescription": "primary missing",
	"interval": {
		"start": 8,
		"end": 8
	},"suggestions": []
}
```

Note that the interval corresponds the the missing part and there are no suggestions.

Here is another example that would cause suggestions in the case of changes (see example [here](/docs/api/changes/post.md#compilation-errors)), but does not using this basic operation (i.e., since it is supposed that there is no support for changes):

```json
{
	"category": "examples",
	"package": "Webside",
	"sourceCode": "xt ^x * t"
}
```

Then the response should be like this:

```json
409
{
	"description": "undeclared t",
	"fullDescription": "undeclared identifier t at line 2 position 2",
	"interval": {
		"start": 4,
		"end": 4
	},
	"suggestions": []
}
```
