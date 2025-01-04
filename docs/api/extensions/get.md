# Retrieve extensions

These are extensions that apply or are meant to be used specifically by the IDE. The gaining here is that each especific backend (dialect) can benefit from this endpoint to populate the Webside with options offered in native IDEs.

## Target object

An extension is applicable to a given meta-model object: package, class, variable, category, method or code. There is also a special type `system` for those extensions that are general for the IDE.

## Specification

The basic structure of an extension specification must be like this:

```json
{
	"extensionType": "string",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"section": "string"
}
```

Where:

-   `extensionType` this property can be `change`, `export` or `search`.
-   `elementType` represents the meta-model object over which the change applies. It can be `system`, `package`, `class`, `variable`, `category`, `method` or `code`.
-   `label` a text that will be used by the IDE to present the option.
-   `description` (optional) a description of the search (it might be used as a tip text or help).
-   `section` (optional) is used by the IDE to place the option under a submenu. By default no submenu is used (i.e., the option will be appended to the corresponding menu)

Additionally, every type of extension might impose other particular properties.

**URL**: `/extensions`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

There are the following types of extensions:

-   [**Change extensions**](changes.md)
-   [**Export extensions**](exports.md)
-   [**Search extensions**](searches.md)
