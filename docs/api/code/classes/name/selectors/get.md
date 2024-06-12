# Retrieve selectors

Retrieve selectors of a given class.

**URL**: `/class/{name}/selectors`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[string]`

**Example:**: `Point class` selectors `GET /classes/Point class/selectors`.

```json
[
	"x:y:",
	"approvedSelectorsForMethodFinder",
	"r:degrees:",
	"r:theta:",
	"settingInputWidgetForNode:"
]
```
