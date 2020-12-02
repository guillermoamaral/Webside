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
## Endpoints

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/dialect](code/dialect/get.md) | - | - | - |
| GET | [/methods](code/methods/get.md) | Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided) | selector<br />sending<br />class<br />referencingClass<br />cagegory<br />referencingVariable<br />usingVariable<br />assigningVariable | | - |
| GET | [/classes](code/classes/get.md) | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy | root<br />names<br />tree<br />depth | | - |
| GET | [/classes/{name}/variables](code/classes/variables/get.md) | - | - | - |
| GET | [/classes/{name}/instance-variables](code/classes/instance-variables/get.md) | - | - | - |
| GET | [/classes/{name}/class-variables](code/classes/class-variables/get.md) | - | - | - |
| GET | [/classes/{name}/categories](code/classes/categories/get.md) | - | - | - |
| GET | [/classes/{name}/methods](code/classes/methods/get.md) | - | - | - |
| GET | [/classes/{name}/projects](code/projects/get.md) | - | - | - |