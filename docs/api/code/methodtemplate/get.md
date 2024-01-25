# Provide a method template

Provide a template for creating a method. This relies on the backend as different dialect might have different templates.
This template is not but a method as specified in [methods/{selector}](../methods/get.md), with an extra property `template` indicating that is not an actual method.

**URL**: `/methodtemplate`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `class`

**Example:**: method template from Pharo 11 `GET /methodtemplate`.

```json
{
	"template": true,
	"selector": "methodSelectorAndArgumentNames",
	"source": "methodSelectorAndArgumentNames\r\t\"comment stating purpose of instance-side method\"\r\t\"scope: class-variables  &  instance-variables\"\r\r\t| temporary variable names |\r\tstatements"
}
```
