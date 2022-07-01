# Packages

While not every Smalltalk name them _packages_, they all manages some sort of code bundles, essentially a named bunch of classes and methods, sometimes global definitions, an additional description, etc.\
There are some aspects however, that are specific of a given Smalltalk. For instance, there are _tags_ in Pharo, an optional _packageVersion_ in Dolphin, or even _children_ in Bee, forming a hierarchy projects (they are called that way).

For the sake of generality, Webside expects this package structure:

```json
{
	"name": "string",
	"description": "boolean",
	"classes": ["string"],
	"methods": {
		"[class name]": ["string"]
	}
}
```

Where `classes` should contain the names of classes defined in the package, and `methods` class extensions grouped by class.

## Endpoints

| Method | Path                                            | Description                               | Parameters | Payload |
| :----: | ----------------------------------------------- | ----------------------------------------- | :--------: | ------- |
|  GET   | [/packages](get.md)                             | Retrive all packages                      |  `names`   | -       |
|  GET   | [/packages/{name}](name/get.md)                 | Retrive a given package                   |     -      | -       |
|  GET   | [/packages/{name}/classes](name/classes/get.md) | Retrive actual classes of a given package |     -      | -       |
|  GET   | [/packages/{name}/methods](name/methods/get.md) | Retrive actual methods of a given package |     -      | -       |
