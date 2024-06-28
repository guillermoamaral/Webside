# Exports extensions

This type of extension allows us to retrieve an _exportable version_ of a given meta-model object. Like other extensions, specifications retrieved from this endpoint will enlarge the list of options available on the IDE.

## Target object

An export extension is applicable to a given meta-model object (package, class, method, etc.). See below accepted element types.

# Specification

The structure of a export specification must be like this:

```json
{
	"extensionType": "export",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"get": "string",
	"defaultFilename": "boolean",
	"section": "string"
}
```

Where:

-   `extensionType` as these are export extensions this property is `export`.
-   `elementType` represents the meta-model object over which the change applies. It can be `package`, `class`, `variable`, `category`, `method` or `code`.
-   `label` a text that will be used by the IDE to present the option.
-   `description` (optional) a description of the exoirt (it might be used as a tip text or help).
-   `get` a string specifying the way the export content will be obtained (see below).
-   `defaultFilename` (optional) the default file name.
-   `section` (optional) is used by the IDE to place the option under a submenu. By default no submenu is used (i.e., the option is just appended to the corresponding menu)

## Example

Lets suppose we want to add a "File out" option to classes and that we count on a URI for that (`/classes/{classname}/exports/chunks` for instance).\
We could specify our extension like this:

```json
{
	"extensionType": "export",
	"elementType": "class",
	"label": "File out",
	"get": "/classes/{element.name}/exports/chunks",
	"defaultFilename": "{element.name}.ch"
}
```

## Get mechanism

As mentioned above, `get` specifies the way the IDE will get the actual contents to be exported. This could be an URI (if it begins with `/`) or an attribute of the target element.

In both cases, the value might contain `{element.xxx}` expressions, where `element` represents the meta-model object for which the export is requested (most likely the object selected in IDE), and `xxx` is an valid attribute for such element. This expression will be replaced by the actual value element's attribute.\
In the example, assuming the action was triggered from the class `MyClass`, the contents will be retrieved from the URI `/classes/MyClass/exports/chunks`.

Valid attributes can be extracted from the corresponding endpoints in the documentation ([pacakges](../code/packages/get.md), [classes](../code/classes/get.md), [variables](../code/classes/name/variables/get.md), [categories](../code/classes/name/categories/get.md), [methods](../code/methods/get.md)).

### Default file name

As in the case of `get` property, `defaultFilename` is a string that can contain zero or more expression of the form `{element.xxx}` that will be replaced by their actual value.

In our example, the default file name will be `MyClass.ch`.

Assuming the payload of requesting `/classes/MyClass/exports/chunks` is the follwing:

```
Object subclass: #MyClass
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'!

!MyClass methodsFor: 'as yet unclassified' stamp: 'guille 3/31/2024 16:34'!
myMethod
  ^1! !
```

A file dialog will be opened and once the user confirms a file name, such contents will be saved.
