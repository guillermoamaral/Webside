# API
Webside is based on a Smalltalk system implementing the following web API.

It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

# Sections
These are the different sections of the API.

_Note: URL path does not include the base URI. The base URI targeting a particular Smalltalk system will be prompted at the moment of opening Webside._

| Section | Method | Path | Description | Parameters | Payload |
| -- | :--: | -- | -- | :--: | -- |
| [Code](code) | GET | [/dialect](code/get-dialect.md) | - | - | | - |
| [Code](code) | GET | [/methods](code/methods/get.md) | Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided) | selector<br />sending<br />class<br />referencingClass<br />cagegory<br />referencingVariable<br />usingVariable<br />assigningVariable | | - |
| [Code](code) | GET | [/classes](code/classes/get.md) | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy | root<br />names<br />tree<br />depth | | - |
routeGET: '/dialect' to: #dialect;
| [Code](code) | GET | [/classes/{name}/variables](code/classes/variables/get.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/instance-variables](code/classes/instance-variables/get.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/class-variables](code/classes/class-variables/get.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/categories](code/classes/categories/get.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/methods](code/classes/methods/get.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/projects](code/projects/get.md) | - | - | | - |
| [Changes](changes) | GET | [/classes/changes](changes/get.md) | - | - | | - |
| [Changes](changes) | POST | [/classes/changes](changes/post.md) | - | - | | - |
| [Evaluations](evaluations) | POST | [/evaluations](evaluations/post.md) | - | - | | - |
| [Objects](objects) | GET | [/objects](objects/get.md) | - | - | | - |
| [Objects](objects) | GET | [/objects/{id}](objects/get-id.md) | - | - | | - |
| [Objects](objects) | DELETE | [/objects](objects/delete.md) | - | - | | - |
| [Objects](objects) | GET | [/objects/{id}/*](objects/get-slot.md) | - | - | | - |
| [Workspaces](workspaces) | GET | [/workspaces](workspaces/get.md) | - | - | | - |
| [Workspaces](workspaces) | POST | [/workspaces](workspaces/post.md) | - | - | | - |
| [Workspaces](workspaces) | GET | [/workspaces/{id}](workspaces/get-id.md) | - | - | | - |
| [Workspaces](workspaces) | DELETE | [/workspaces/{id}](workspaces/delete.md) | - | - | | - |
| [Debugging](debugging) | POST | [/debuggers/{id}](debuggers/post.md) | - | - | | - |
| [Debugging](debugging) | GET | [/debuggers/{id}/frames](debuggers/frames/get.md) | - | - | | - |
| [Debugging](debugging) | GET | [/debuggers/{id}/frames/{index}](debuggers/frame/get.md) | - | - | | - |
| [Debugging](debugging) | GET | [/debuggers/{id}/frames/{index}/bindings](debuggers/frame/bindings/get.md) | - | - | | - |
| [Debugging](debugging) | POST | [/debuggers/{id}/skip](debuggers/skip.md) | - | - | | - |
| [Debugging](debugging) | POST | [/debuggers/{id}/hop](debuggers/hop.md) | - | - | | - |
| [Debugging](debugging) | POST | [/debuggers/{id}/restart](debuggers/restart.md) | - | - | | - |
| [Debugging](debugging) | POST | [/debuggers/{id}/resume](debuggers/resume.md) | - | - | | - |
| [Debugging](debugging) | POST | [/debuggers/{id}/terminate](debuggers/terminate.md) | - | - | | - |
| [Debugging](debugging) | DELETE | [/debuggers/{id}](debuggers/delete.md) | - | - | | - |