# API
Webside is based on a Smalltalk system implementing the following web API.

It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

# Sections
These are the different sections of the API.

_Note: URL path does not include the base URI. The base URI targeting a particular Smalltalk system will be prompted at the moment of opening Webside._

* [Code](code)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/dialect](code/get-dialect.md) | - | - | | - |
| GET | [/methods](code/methods/get.md) | Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided) | selector<br />sending<br />class<br />referencingClass<br />cagegory<br />referencingVariable<br />usingVariable<br />assigningVariable | | - |
| GET | [/classes](code/classes/get.md) | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy | root<br />names<br />tree<br />depth | | - |
| GET | [/classes/{name}/variables](code/classes/variables/get.md) | - | - | | - |
| GET | [/classes/{name}/instance-variables](code/classes/instance-variables/get.md) | - | - | | - |
| GET | [/classes/{name}/class-variables](code/classes/class-variables/get.md) | - | - | | - |
| GET | [/classes/{name}/categories](code/classes/categories/get.md) | - | - | | - |
| GET | [/classes/{name}/methods](code/classes/methods/get.md) | - | - | | - |
| GET | [/classes/{name}/projects](code/projects/get.md) | - | - | | - |

* [Changes](changes)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/classes/changes](changes/get.md) | - | - | | - |
| POST | [/classes/changes](changes/post.md) | - | - | | - |

* [Evaluations](evaluations)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/evaluations](evaluations/post.md) | - | - | | - |

* [Objects](objects)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/objects](objects/get.md) | - | - | | - |
| GET | [/objects/{id}](objects/get-id.md) | - | - | | - |
| DELETE | [/objects](objects/delete.md) | - | - | | - |
| GET | [/objects/{id}/*](objects/get-slot.md) | - | - | | - |

* [Workspaces](workspaces)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/workspaces](workspaces/get.md) | - | - | | - |
| POST | [/workspaces](workspaces/post.md) | - | - | | - |
| GET | [/workspaces/{id}](workspaces/get-id.md) | - | - | | - |
| DELETE | [/workspaces/{id}](workspaces/delete.md) | - | - | | - |

* [Debugging](debugging)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/debuggers/{id}](debuggers/post.md) | - | - | | - |
| GET | [/debuggers/{id}/frames](debuggers/frames/get.md) | - | - | | - |
| GET | [/debuggers/{id}/frames/{index}](debuggers/frame/get.md) | - | - | | - |
| GET | [/debuggers/{id}/frames/{index}/bindings](debuggers/frame/bindings/get.md) | - | - | | - |

* [Debugging](debugging)

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/debuggers/{id}/skip](debuggers/skip.md) | - | - | | - |
| POST | [/debuggers/{id}/hop](debuggers/hop.md) | - | - | | - |
| POST | [/debuggers/{id}/restart](debuggers/restart.md) | - | - | | - |
| POST | [/debuggers/{id}/resume](debuggers/resume.md) | - | - | | - |
| POST | [/debuggers/{id}/terminate](debuggers/terminate.md) | - | - | | - |
| DELETE | [/debuggers/{id}](debuggers/delete.md) | - | - | | - |