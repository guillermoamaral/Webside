# API
Webside is based on a Smalltalk system implementing the following web API.

It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

# Sections
These are the different sections of the API.

_Note: URL path does not include the base URI. The base URI targeting a particular Smalltalk system will be prompted at the moment of opening Webside._

* [Code](code): Endpoints to retrieve the code (projects, classes and methods).

* [Changes](changes): Endpoints to change code (projects, classes and methods).

* [Evaluations](evaluations): Endpoints to evaluate, debug and profile expressions.

* [Objects](objects): Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

* [Workspaces](workspaces): Endpoints to manage workspaces.

* [Debugging](debugging): Endpoints to manage debuggers and interact with them.

* [Processes](processes): Endpoints to manage active processes.

* [Tests](tests): Endpoints to run tests and retrieve their results.

* [Profiling](profiling): Endpoints to manage profilers and access their results.

| Section | Method | Path | Description | Parameters | Payload |
| -- | -- | -- | -- | -- | -- 
| [Code](code) | GET | [/methods](code/methods/get.md) | Retrieve those methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided) | selector<br />sending<br />class<br />referencingClass<br />cagegory<br />referencingVariable<br />usingVariable<br />assigningVariable | | - |
| [Code](code) | GET | [/classes](code/classes/get.md) | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy | root<br />names<br />tree<br />depth | | - |
| [Code](code) | GET | [/classes{name}variables](code/classes/get-variables.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/categories](code/classes/get-categories.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/methods](code/classes/get-methods.md) | - | - | | - |
| [Code](code) | GET | [/classes/{name}/projects](code/projects/get.md) | - | - | | - |