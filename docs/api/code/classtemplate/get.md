# Retrieve class template

Provide a template for creating a class. This relies on the backend as different dialect might have different templates.
This template is not but a class as specified in [classses/{name}](../classes/name/get.md), with an extra property `template` indicating that is not an actual class.
As some dialects might impose a package for a class, this endpoint requires a `package` on the query.

**URL**: `/classtemplate`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `class`

**Example:**: class template from Pharo 11 `GET /classtemplate?package=MyPackage`.

```json
{
	"template": true,
	"name": "MyClass",
	"definition": "Object\r\tsubclass: #MyClass\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpackage: 'MyPackage'"
}
```
