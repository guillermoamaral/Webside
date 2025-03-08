# Search extensions

This type of extension allows us to define and perform specific searches (method searches by the moment) on the backend. Like other extensions, these ones will enlarge the list of options available on the IDE.

## Specification

A search extension must be like this:

```json
{
	"type": "search",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"get": "string",
	"section": "string"
}
```

Where:

-   `type` this property should be `search`.
-   `get` the URI to perform the actual search (see below).

For basic properties refer to [basic extension properties](./get.md#specification).

## Example

Lets suppose we want to add a "Unary methods" search option to packages and that we count on a endpoint for that (`/packages/{packagename}/unaries` for instance), that searches for unary methods on a given package.\
We could specify our extension like this:

```json
{
	"type": "search",
	"elementType": "package",
	"label": "Unary methods",
	"get": "/packages/{element.name}/unaries"
}
```

## Get mechanism

`get` property should be the URI corresponding to the endpoint associated to the search of interest.
The value might contain `{element.xxxx}` expressions, where `element` represents the meta-model object for which the search is requested (most likely the object selected in IDE), and `xxxx` is an valid attribute for such element. This expression will be replaced by the actual value element's attribute.\

In the example, assuming the action was triggered from the package `MyPackage`, the contents will be retrieved from the URI `/pacakges/MyPackage/unaries`.

Valid attributes can be extracted from the corresponding endpoints in the documentation ([pacakges](../code/packages/get.md), [classes](../code/classes/get.md), [variables](../code/classes/name/variables/get.md), [categories](../code/classes/name/categories/get.md), [methods](../code/methods/get.md)).
