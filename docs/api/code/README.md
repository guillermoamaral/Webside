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
| GET | [/dialect](dialect/get.md) | - | - | - |
| GET | [/methods](methods/get.md) | Retrieve methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided) | selector<br />sending<br />class<br />referencing<br />category<br />using<br />assigning | | - |
| GET | [/classes](classes/get.md) | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy | root<br />names<br />tree<br />depth | - |
| GET | [/classes/{name}](classes/name/get.md) | Retrieve the class with a given name | - | - |
| GET | [/classes/{name}/variables](classes/name/variables/get.md) | Retrive instance and class variables of a given class | - | - |
| GET | [/classes/{name}/instance-variables](classes/name/instance-variables/get.md) | Retrive instance variables of a given class | - | - |
| GET | [/classes/{name}/class-variables](classes/name/class-variables/get.md) | Retrive class variables of a given class | - | - |
| GET | [/classes/{name}/categories](classes/name/categories/get.md) | Retrive categories of a given class | - | - |
| GET | [/classes/{name}/methods](classes/name/methods/get.md) | Retrive methods of a given class | - | - |
| GET | [/projects](projects/get.md) | Retrive all projects of a given root project (if no root is provided, the uppermost project in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the projects hierarchy | root<br />names<br />tree<br />depth | - |