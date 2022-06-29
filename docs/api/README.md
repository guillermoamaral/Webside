# API

Webside is based on a Smalltalk system implementing the following web API.

It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

The implementation of API calls, including helper services, is centralized in a class called [API](../../client/src/components/API.js).

# Sections

These are the different sections of the API.

_Note: URL path does not include the base URI. The base URI targeting a particular Smalltalk system will be prompted at the moment of opening Webside._

## [Code](code)

These are the endpoints used to retrieve system code: packages, classes and methods.

| Method | Path                                                                              | Description                                                                                                                                                                                                             |                                       Parameters                                        | Payload |
| :----: | --------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------: | ------- |
|  GET   | [/dialect](code/dialect/get.md)                                                   | -                                                                                                                                                                                                                       |                                            -                                            | -       |
|  GET   | [/methods](code/methods/get.md)                                                   | Retrieve methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided)                                                                                         | selector<br />sending<br />class<br />accessing<br />category<br />using<br />assigning | -       |
|  GET   | [/classes](code/classes/get.md)                                                   | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy |                          root<br />names<br />tree<br />depth                           | -       |
|  GET   | [/classes/{name}](code/classes/name/get.md)                                       | Retrieve a given class                                                                                                                                                                                                  |                                            -                                            | -       |
|  GET   | [/classes/{name}/variables](code/classes/name/variables/get.md)                   | Retrive instance and class variables of a given class                                                                                                                                                                   |                                            -                                            | -       |
|  GET   | [/classes/{name}/instance-variables](code/classes/name/instance-variables/get.md) | Retrive instance variables of a given class                                                                                                                                                                             |                                            -                                            | -       |
|  GET   | [/classes/{name}/class-variables](code/classes/name/class-variables/get.md)       | Retrive class variables of a given class                                                                                                                                                                                |                                            -                                            | -       |
|  GET   | [/classes/{name}/categories](code/classes/name/categories/get.md)                 | Retrive categories of a given class                                                                                                                                                                                     |                                            -                                            | -       |
|  GET   | [/classes/{name}/methods](code/classes/name/methods/get.md)                       | Retrive methods of a given class                                                                                                                                                                                        |                                            -                                            | -       |
|  GET   | [/packages](code/packages/get.md)                                                 | Retrive all packages                                                                                                                                                                                                    |                          root<br />names<br />tree<br />depth                           | -       |
|  GET   | [/packages/{name}](code/packages/name/get.md)                                     | Retrive a given package                                                                                                                                                                                                 |                                            -                                            | -       |

## [Changes](changes)

Endpoints to apply changes and retrieve changes made to the system.

| Method | Path                        | Description                         | Parameters | Payload |
| :----: | --------------------------- | ----------------------------------- | :--------: | ------- |
|  GET   | [/changes](changes/get.md)  | Retrieve changes made to the system |   author   | -       |
|  POST  | [/changes](changes/post.md) | Apply a change to the system        |     -      | -       |

## [Evaluations](evaluations)

Endpoints to evaluate expressions and manage active evaluations.

| Method | Path                                        | Description                             | Parameters | Payload      |
| :----: | ------------------------------------------- | --------------------------------------- | :--------: | ------------ |
|  POST  | [/evaluations](evaluations/post.md)         | Evaluate an expression                  |     -      | `evaluation` |
|  GET   | [/evaluations](evaluations/get.md)          | Retrieve active evaluations             |     -      | -            |
|  GET   | [/evaluations/id](evaluations/id/get.md)    | Retrieve the evaluation with a given ID |     -      | -            |
| DELETE | [/evaluations/id](evaluations/id/delete.md) | Cancel the evaluation with a given ID   |     -      | -            |

## [Objects](objects)

Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

| Method | Path                                        | Description                                   | Parameters | Payload      |
| :----: | ------------------------------------------- | --------------------------------------------- | :--------: | ------------ |
|  GET   | [/objects](objects/get.md)                  | Retrive pinned objects                        |     -      | -            |
|  GET   | [/objects/{id}](objects/id/get.md)          | Retrieve the pinned object with a given ID    |     -      | -            |
| DELETE | [/objects/{id}](objects/id/delete.md)       | Unpin the object with a given ID              |     -      | -            |
|  GET   | [/objects/{id}/\*](objects/id/slots/get.md) | Retrive the object reached thru the given URI |     -      | -            |
|  POST  | [/objects](objects/post.md)                 | Pin the object reached thru the given URI     |     -      | `object URI` |

## [Workspaces](workspaces)

Endpoints to manage workspaces.

| Method | Path                                        | Description                            | Parameters | Payload |
| :----: | ------------------------------------------- | -------------------------------------- | :--------: | ------- |
|  GET   | [/workspaces](workspaces/get.md)            | Retrieve active workspaces             |     -      | -       |
|  POST  | [/workspaces](workspaces/post.md)           | Create a new workspace                 |     -      | -       |
|  GET   | [/workspaces/{id}](workspaces/id/get.md)    | Retrieve the workspace with a given ID |     -      | -       |
| DELETE | [/workspaces/{id}](workspaces/id/delete.md) | Delete the workspece with a given ID   |     -      | -       |

## [Debuggers](debuggers)

Endpoints to manage debuggers and interact with them.

| Method | Path                                                                                 | Description                                                                    | Parameters | Payload                      |
| :----: | ------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------ | :--------: | ---------------------------- |
|  GET   | [/debuggers](debuggers/get.md)                                                       | Retrieve open debuggers                                                        |     -      | -                            |
|  POST  | [/debuggers](debuggers/post.md)                                                      | Create a debugger upon an given process (evaluation)                           |     -      | `{ "evaluation": "string" }` |
|  GET   | [/debuggers/{id}/frames](debuggers/id/frames/get.md)                                 | Retrieve frames of the debugger with a given ID                                |     -      | -                            |
|  GET   | [/debuggers/{id}/frames/{index}](debuggers/id/frames/index/get.md)                   | Retrieve the _i_-th frame withing the debugger with a given ID                 |     -      | -                            |
|  GET   | [/debuggers/{id}/frames/{index}/bindings](debuggers/id/frames/index/bindings/get.md) | Retrieve the bindings of the _i_-th frame withing the debugger with a given ID |     -      | -                            |
|  POST  | [/debuggers/{id}/stepover](debuggers/id/stepover/post.md)                            | Step over the current sentence in the debugger with a given ID                 |     -      | -                            |
|  POST  | [/debuggers/{id}/stepinto](debuggers/id/stepinto/post.md)                            | Step into the current sentence in the debugger with a given ID                 |     -      | -                            |
|  POST  | [/debuggers/{id}/restart](debuggers/id/restart/post.md)                              | Restart the debugger with a given ID                                           |     -      | -                            |
|  POST  | [/debuggers/{id}/resume](debuggers/id/resume/post.md)                                | Resume the process of the debugger with a given ID                             |     -      | -                            |
|  POST  | [/debuggers/{id}/terminate](debuggers/id/terminate/post.md)                          | Terminate process being debugged and closesthe debugger with a given ID        |     -      | -                            |
| DELETE | [/debuggers/{id}](debuggers/id/delete.md)                                            | Closes the debugger with a given ID (terminating the process being debugged)   |     -      | -                            |

## [Testing](test-runs)

Endpoints to run tests and retrieve their results.

| Method | Path                                                   | Description                                    | Parameters | Payload                                                                 |
| :----: | ------------------------------------------------------ | ---------------------------------------------- | :--------: | ----------------------------------------------------------------------- |
|  POST  | [/test-runs](test-runs/post.md)                        | Create and run of a test suite                 |     -      | `{ "methods": ["string"], "classes": ["string"], "package": "string" }` |
|  GET   | [/test-runs/{id}](test-runs/id/get.md)                 | Retrieve the status of a given test suite run  |     -      | -                                                                       |
|  GET   | [/test-runs/{id}/results](test-runs/id/resutls/get.md) | Retrieve the restuls of a given test suite run |     -      | -                                                                       |
|  POST  | [/test-runs/{id}/run](test-runs/id/run/post.md)        | Re-run a given test suite                      |     -      | -                                                                       |
|  POST  | [/test-runs/{id}/debug](test-runs/id/debug/post.md)    | Debug a test withing a test suite run          |     -      | -                                                                       |
|  POST  | [/test-runs/{id}/stop](test-runs/id/stop/post.md)      | Stop an active test suite run                  |     -      | -                                                                       |
| DELETE | [/test-runs/{id}](test-runs/id/delete.md)              | Delete a test suite run                        |     -      | -                                                                       |

## [Profiling](profilers)

Endpoints to manage profilers and access their results.

| Method | Path                                                   | Description                                      | Parameters | Payload |
| :----: | ------------------------------------------------------ | ------------------------------------------------ | :--------: | ------- |
|  GET   | [/profilers/{id}/tree](profilers/id/tree/get.md)       | Retrieve a tree-like results of a given profiler |     -      | -       |
|  GET   | [/profilers/{id}/ranking](profilers/id/ranking/get.md) | Retrieve ranked results of a given profiler      |     -      | -       |
| DELETE | [/profilers/{id}](profilers/id/delete.md)              | Delete a given profiler                          |     -      | -       |
