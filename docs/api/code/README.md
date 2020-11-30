# Code
These are the endpoints used to retrieve system code: projects, classes and methods.

It is important to mention that the JSON objects returned by these endpoints should include common properties of every `object`. This is used by Webside IDE to provide inspection services over objects of the meta-model (classes, methods, etc.).
These are the common properties at the moment:

```json
{
    "class": "string",
    "indexable": "boolean",
    "size": "number",
    "printString": "string"
}
```

## Methods
Endpoints to retrieve methods.

* [Retrieve methods](methods/get.md): `GET /methods`

## Classes
Endpoints to retrieve class definitions, their variables, categories and methods.

* [Retrieve classes](classes/get.md): `GET /classes`
* [Retrieve variables](classes/variables/get.md): `GET /classes/{name}/variables`
* [Retrieve instance variables](classes/instance-variables/get.md): `GET /classes/{name}/instance-variables`
* [Retrieve class variables](classes/class-variables/get.md): `GET /classes/{name}/class-variables`
* [Retrieve categories](classes/categories/get.md): `GET /classes/{name}/categories`
* [Retrieve methods](classes/methods/get.md): `GET /classes/{name}/methods`

## Projects
Endpoints to retrive project definitions.

* [Retrieve projects](projects/get.md): `GET /projects`