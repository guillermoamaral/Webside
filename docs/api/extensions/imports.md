# Import extensions

This type of extension allows us to process an _exportable version_ of a given meta-model object. Like other extensions, specifications retrieved from this endpoint will enlarge the list of options available on the IDE.

## Specification

An import extension must be like this:

```json
{
	"type": "export",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"post": "string",
	"defaultFilename": "boolean",
	"section": "string"
}
```

Where:

-   `type` as these are export extensions this property is `import`.
-   `post` a string specifying the endpoint at which the import content will be processed (see below).
-   `defaultFilename` (optional) the default file name.

The rest of the basic properties are described [here](./get.md#specification).

## Example

Lets suppose we want to add a "File in" option to the system and that we count on a URI for that (`/fileins` for instance).\
We could specify our extension like this:

```json
{
	"type": "import",
	"elementType": "class",
	"label": "File in",
	"post": "/fileins",
	"defaultFilename": "fileout.st"
}
```

## IDE behavior

As this type of extensions enable bringing external stuff into the system, the IDE will first ask the user for a file (opening a file dialog). Once the user selects a file, the content of such file will be sent to the user in the payload of a request to the endpoint specified in the `post` property.

## Post property

As mentioned above, `post` specifies the endpoint that will actually process the contents of the selected file (in the example, the contents will be sent to `/fileins`).

### Default file name

`defaultFilename` can contain zero or more expression of the form `{element.xxxx}`, where `element` represents the meta-model object for which the export is requested (most likely the object selected in IDE), and `xxxx` is an valid attribute for such element. This expression will be replaced by the actual value element's attribute.\

In our example, the it is a fixed string `fileout.st` without _variable parts.

Assuming the file selected contains the follwing:

```
Object subclass: #MyClass
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'!

!MyClass methodsFor: 'as yet unclassified' stamp: 'guille 3/31/2024 16:34'!
myMethod
  ^1! !
```

and that the enpoint `/fileins` actually files in the payload, the import will result in having `MyClass` class installed the system.