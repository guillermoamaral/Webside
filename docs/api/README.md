# API
Webside is based on a Smalltalk system implementing the following web API.

It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

The implementation of API calls, including helper services, is centralized in a class called [API](client/src/components/API.js).

# Sections
These are the different sections of the API.

_Note: URL path does not include the base URI. The base URI targeting a particular Smalltalk system will be prompted at the moment of opening Webside._

## [Code](code)
These are the endpoints used to retrieve system code: projects, classes and methods.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/dialect](code/dialect/get.md) | - | - | - |
| GET | [/methods](code/methods/get.md) | Retrieve methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided) | selector<br />sending<br />class<br />referencingClass<br />cagegory<br />referencingVariable<br />usingVariable<br />assigningVariable | | - |
| GET | [/classes](code/classes/get.md) | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy | root<br />names<br />tree<br />depth | - |
| GET | [/classes/{name}](code/classes/name/get.md) | Retrieve the class with a given name | - | - |
| GET | [/classes/{name}/variables](code/classes/name/variables/get.md) | Retrive instance and class variables of a given class | - | - |
| GET | [/classes/{name}/instance-variables](code/classes/name/instance-variables/get.md) | Retrive instance variables of a given class | - | - |
| GET | [/classes/{name}/class-variables](code/classes/name/class-variables/get.md) | Retrive class variables of a given class | - | - |
| GET | [/classes/{name}/categories](code/classes/name/categories/get.md) | Retrive categories of a given class | - | - |
| GET | [/classes/{name}/methods](code/classes/name/methods/get.md) | Retrive methods of a given class | - | - |
| GET | [/projects](code/projects/get.md) | Retrive all projects of a given root project (if no root is provided, the uppermost project in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the projects hierarchy | root<br />names<br />tree<br />depth | - |

## [Changes](changes)
Endpoints to apply changes and retrieve changes made to the system.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/changes](changes/get.md) | Retrieve changes made to the system | author | | - |
| POST | [/changes](changes/post.md) | Apply a change to the system | - | - |

## [Evaluations](evaluations)
Endpoints to evaluate expressions.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/evaluations](evaluations/post.md) | Evaluates an expression | - | | ```json { "expression": "string" }``` |

## [Objects](objects)
Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/objects](objects/get.md) | Retrive pinned objects | - | - |
| GET | [/objects/{id}](objects/id/get.md) | Retrieve the pinned object with a given ID | - | - |
| DELETE | [/objects/{id}](objects/id/delete.md) | Unpin the object with a given ID | - | - |
| GET | [/objects/{id}/*](objects/id/slots/get.md) | Retrive an inner slot (a any depth) starting from the pinned object with ID | - | - |

## [Workspaces](workspaces)
Endpoints to manage workspaces.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/workspaces](workspaces/get.md) | Retrieve active workspaces | - | - |
| POST | [/workspaces](workspaces/post.md) | Create a new workspace | - | - |
| GET | [/workspaces/{id}](workspaces/get-id.md) | Retrieve the workspace with a given ID | - | - |
| DELETE | [/workspaces/{id}](workspaces/delete.md) | Delete the workspece with a given ID | - | - |

## [Debugging](debugging)
Endpoints to manage debuggers and interact with them.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/debuggers](debuggers/post.md) | Create a debugger upon an given process (evaluation) | - | ```json { "process": "string" }``` |
| GET | [/debuggers/{id}/frames](debuggers/frames/get.md) | Retrieve frames of the debugger with a given ID | - | - |
| GET | [/debuggers/{id}/frames/{index}](debuggers/frame/get.md) | Retrieve the i-th frame withing the debugger with a given ID | - | - |
| GET | [/debuggers/{id}/frames/{index}/bindings](debuggers/frame/bindings/get.md) | Retrieve the bindings of the i-th frame withing the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/skip](debuggers/skip.md) | Skip the current sentence in the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/hop](debuggers/hop.md) | Goes into the current sentence in the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/restart](debuggers/restart.md) | Restart the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/resume](debuggers/resume.md) | Resume the process of the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/terminate](debuggers/terminate.md) | Terminate process being debugged and closesthe debugger with a given ID | - | - |
| DELETE | [/debuggers/{id}](debuggers/delete.md) | Closes the debugger with a given ID (terminating the process being debugged) | - | - |

## [Testing](tests)
Endpoints to run tests and retrieve their results.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |

## [Profiling](profiling)
Endpoints to manage profilers and access their results.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
