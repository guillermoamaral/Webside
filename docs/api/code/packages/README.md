# Packages

While not every Smalltalk call them _packages_, they all manage some sort of code bundles, essentially a named bunch of classes and methods, sometimes global definitions, an additional description, etc.\
There are some aspects however, that are specific to a given Smalltalk. For instance, there are _tags_ in Pharo (class categories), an optional _packageVersion_ in Dolphin, or even _children_ in Bee, forming a *project hierarchy*.

For the sake of generality, Webside expects this package structure:

```json
{
	"name": "string",
	"description": "boolean",
	"classes": ["string"],
	"methods": {
		"[classname]": ["string"]
	}
}
```

Where
- `classes` is the names of the classes defined in the package, and
- `methods` is the list of extensions, grouped by class name.

## Endpoints

| Method | Path                                            | Description                                |               Parameters                | Payload |
| :----: | ----------------------------------------------- | ------------------------------------------ | :-------------------------------------: | ------- |
|  GET   | [/packages](get.md)                             | Retrieve all packages                      |                 `names`                 | -       |
|  GET   | [/packages/{name}](name/get.md)                 | Retrieve a given package                   |                    -                    | -       |
|  GET   | [/packages/{name}/classes](name/classes/get.md) | Retrieve actual classes of a given package | `extended`, `tree`, `names`, `category` | -       |
|  GET   | [/packages/{name}/methods](name/methods/get.md) | Retrieve actual methods of a given package |                    -                    | -       |
