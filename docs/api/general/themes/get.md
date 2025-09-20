# Themes

This endpoint exposes system themes, a list of custom color sets.

**URL**: `/themes`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[theme]` where `theme` is defined as:

```json
{
	"name": "string",
	"light": "styles",
	"dark": "styles"
}
```

Where `styles` is an object with the following properties:

```json
{
	"selectorStyle": "style",
	"symbolStyle": "style",
	"argumentStyle": "style",
	"temporaryStyle": "style",
	"assignmentStyle": "style",
	"stringStyle": "style",
	"variableStyle": "style",
	"metaStyle": "style",
	"bracketStyle": "style",
	"reservedStyle": "style",
	"returnStyle": "style",
	"globalStyle": "style",
	"numberStyle": "style",
	"commentStyle": "style"
}
```

and `style` has the form:

```json
{
	"color": "string",
	"italic": "boolean",
	"bold": "boolean"
}
```

with `color` being an RGBA color in HEX format.

If style is omitted a default style will be used.

**Example:**: Pharo themes `GET /themes`.

```json
[
	{
		"name": "Tango",
		"light": {
			"commentStyle": {
				"color": "#888a85"
			},
			"returnStyle": {
				"color": "#000000",
				"bold": true
			},
			"variableStyle": {
				"color": "#204a87"
			},
			"temporaryStyle": {
				"color": "#888a85",
				"italic": true
			},
			"argumentStyle": {
				"color": "#555753",
				"italic": true
			},
			"symbolStyle": {
				"color": "#c4a000"
			},
			"stringStyle": {
				"color": "#ce5c00"
			},
			"globalStyle": {
				"color": "#5c3566",
				"bold": true
			},
			"numberStyle": {
				"color": "#8f5902"
			},
			"selectorStyle": {
				"color": "#000000",
				"bold": true
			},
			"reservedStyle": {
				"color": "#8F5902"
			}
		}
	}
]
```
