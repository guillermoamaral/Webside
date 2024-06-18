# Search extensions

This type of extension allows us to define and perform specific searches (method searches by the moment) on the backend. Like other extensions, these ones will enlarge the list of options available on the IDE.

## Target object

A search extension is applicable to a given meta-model object (package, class, method, etc.). See below accepted element types.

# Specification

The structure of a search specification must be like this:

```json
{
	"extensionType": "search",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"get": "string",
	"section": "string"
}
```

Where:

-   `extensionType` this property should be `search`.
-   `elementType` represents the meta-model object over which the change applies. It can be `package`, `class`, `variable`, `category`, `method` or `code`.
-   `label` a text that will be used by the IDE to present the option.
-   `description` (optional) a description of the search (it might be used as a tip text or help).
-   `get` the URI to perform the actual search (see below).
-   `section` (optional) is used by the IDE to place the option under a submenu. By default no submenu is used (i.e., the option is just appended to the corresponding menu)

## Example

Lets suppose we want to add a "Unary methods" search option to packages and that we count on a endpoint for that (`/packages/{packagename}/unaries` for instance), that searches for unary methods on a given package.\
We could specify our extension like this:

```json
{
	"extensionType": "search",
	"elementType": "package",
	"label": "Unary methods",
	"get": "/packages/{element.name}/unaries"
}
```

## Get mechanism

`get` property should be the URI corresponding to the endpoint associated to the search of interest.
The value might contain `{element.xxx}` expressions, where `element` represents the meta-model object for which the search is requested (most likely the object selected in IDE), and `xxx` is an valid attribute for such element. This expression will be replaced by the actual value element's attribute.\

In the example, assuming the action was triggered from the package `MyPackage`, the contents will be retrieved from the URI `/pacakges/MyPackage/unaries`.

Valid attributes can be extracted from the corresponding endpoints in the documentation ([pacakges](../code/packages/get.md), [classes](../code/classes/get.md), [variables](../code/classes/name/variables/get.md), [categories](../code/classes/name/categories/get.md), [methods](../code/methods/get.md)).
