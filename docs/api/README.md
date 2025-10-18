# API

Webside is based on a Smalltalk system implementing the following web API.

It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

The implementation of API calls, including helper services, is centralized in a class called [API](../../src/components/Backend.js).

# IDs

Resources are identified by a unique ID. The production of these IDs rely on the backend implementation and may vary from dialect to dialect. The only requirement is that they should be unique.
For the sake of simplicity, they will be `1`, `2`, etc. along this documentation.

# Sections

These are the different sections of the API.

_Note: URL path does not include the base URI. The base URI targeting a particular Smalltalk system will be prompted at the moment of opening Webside._

## [General](general)

These are general endpoints that apply to the system as a whole.

| Method | Path                               | Description                | Parameters | Payload |
| :----: | ---------------------------------- | -------------------------- | :--------: | ------- |
|  GET   | [/dialect](general/dialect/get.md) | Retrieve Smalltalk dialect |            | -       |
|  GET   | [/version](general/version/get.md) | Retrieve system version    |            | -       |
|  GET   | [/colors](general/colors/get.md)   | Retrieve system colors     |            | -       |
|  GET   | [/logo](general/logo/get.md)       | Retrieve system logo       |            | -       |
|  GET   | [/stats](general/stats/get.md)     | Retrieve system stats      |            | -       |
|  GET   | [/themes](general/themes/get.md)   | Retrieve custom themes     |            | -       |
|  GET   | [/icons](general/icons/get.md)     | Retrieve system icons      |            | -       |
|  POST  | [/save](general/save/post.md)      | Save the image             |            | -       |

## [Code](code)

These are the endpoints used to retrieve system code: packages, classes and methods.

| Method | Path                                                                              | Description                                                                                                                                                                                                             |                                                                                     Parameters                                                                                      | Payload |
| :----: | --------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | ------- |
|  GET   | [/methods](code/methods/get.md)                                                   | Retrieve methods satisfying the condition specified in the query (or all the methods in the system if no condition is provided)                                                                                         | `selector`, `category`, `accessing`, `using`, `assigning`, `sending`, `referencingClass`, `selectorMatching`, `ast`, `annotations`, `bytecodes`, `dissasembly`, `count`, `modified` | -       |
|  GET   | [/classes](code/classes/get.md)                                                   | Retrieve all subclasses of a given root class (if no root is provided, the uppermost class in the system is used). It is also possible get a tree-like structure as well as to limit the depth in the classes hierarchy |                                                                    `root`, `names`, `tree`, `depth`, `category`                                                                     | -       |
|  GET   | [/classes/{name}](code/classes/name/get.md)                                       | Retrieve a given class                                                                                                                                                                                                  |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/superclasses](code/classes/name/superclasses/get.md)             | Retrieve superclasses a given class                                                                                                                                                                                     |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/subclasses](code/classes/name/subclasses/get.md)                 | Retrieve subclasses a given class                                                                                                                                                                                       |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/variables](code/classes/name/variables/get.md)                   | Retrieve instance and class variables of a given class                                                                                                                                                                  |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/instance-variables](code/classes/name/instance-variables/get.md) | Retrieve instance variables of a given class                                                                                                                                                                            |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/class-variables](code/classes/name/class-variables/get.md)       | Retrieve class variables of a given class                                                                                                                                                                               |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/categories](code/classes/name/categories/get.md)                 | Retrieve categories of a given class                                                                                                                                                                                    |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/used-categories](code/classes/name/used-categories/get.md)       | Retrieve categories used throughout the hierarchy of a given class                                                                                                                                                      |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/selectors](code/classes/name/selectors/get.md)                   | Retrieve selectors of a given class                                                                                                                                                                                     |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/methods](code/classes/name/methods/get.md)                       | Retrieve methods of a given class                                                                                                                                                                                       |                                                                                          -                                                                                          | -       |
|  GET   | [/classes/{name}/methods/{selector}](code/classes/name/methods/selector/get.md)   | Retrieve the method with the given selector in the given class                                                                                                                                                          |                                                                                          -                                                                                          | -       |
|  GET   | [/categories](code/categories/get.md)                                             | Retrieve all categories in the system                                                                                                                                                                                   |                                                                                          -                                                                                          | -       |
|  GET   | [/usual-categories](code/usual-categories/get.md)                                 | Retrieve popular categories used throughout the system                                                                                                                                                                  |                                                                                          -                                                                                          | -       |
|  GET   | [/packages](code/packages/get.md)                                                 | Retrieve all packages                                                                                                                                                                                                   |                                                                                       `names`                                                                                       | -       |
|  GET   | [/packages/{name}](code/packages/name/get.md)                                     | Retrieve a given package                                                                                                                                                                                                |                                                                                          -                                                                                          | -       |
|  GET   | [/packages/{name}/classes](code/packages/name/classes/get.md)                     | Retrieve actual classes of a given package                                                                                                                                                                              |                                                                       `extended`, `tree`, `names`, `category`                                                                       | -       |
|  GET   | [/packages/{name}/methods](code/packages/name/methods/get.md)                     | Retrieve actual methods of a given package                                                                                                                                                                              |                                                                                          -                                                                                          | -       |
|  GET   | [/search](code/search/get.md)                                                     | Perform a search on the system                                                                                                                                                                                          |                                                                      `text`, `ignoreCase`, `position`, `type`                                                                       | -       |
|  GET   | [/autocompletions](code/autocompletions/post.md)                                  | Retrieve completion entries for a given code, position and context                                                                                                                                                      |                                                                                          -                                                                                          | -       |
|  GET   | [/classtemplate](code/classtemplate/get.md)                                       | Provide a class template                                                                                                                                                                                                |                                                                                      `package`                                                                                      | -       |
|  GET   | [/methodtemplate](code/methodtemplate/get.md)                                     | Provide a method template                                                                                                                                                                                               |                                                                                          -                                                                                          | -       |
|  POST  | [/selector](code/selector/post.md)                                                | Find a selector in a piece of source code                                                                                                                                                                               |                                                                                          -                                                                                          | -       |

## [Changes](changes)

Endpoints to apply changes and retrieve changes made to the system.

| Method | Path                                                                                            | Description                                   | Parameters | Payload  |
| :----: | ----------------------------------------------------------------------------------------------- | --------------------------------------------- | :--------: | :------: |
|  GET   | [/changes](changes/get.md)                                                                      | Retrieve changes made to the system           |   author   |    -     |
|  POST  | [/changes](changes/post.md)                                                                     | Apply a change to the system                  |     -      | `change` |
|  GET   | [/classes/{name}/methods/{selector}/history](code/classes/name/methods/selector/history/get.md) | Retrieve historical changes on a given method |     -      |    -     |

### [Basic CRUD operations](changes/crud/README.md)

For the sake of simplicity, the API encourages the use of a single endpoint `/changes` for applying changes to the system.\
However, as this may imply to count on some implementation of _refactoring changes_ on the backend (and might not be the case),
a basic set of CRUD operations on main meta-model objects should be provided.
The data required for these operations should be the same as the one provided for the corresponding changes.

| Method | Path                                                                                       | Description                                                     | Parameters | Payload   |
| :----: | ------------------------------------------------------------------------------------------ | --------------------------------------------------------------- | :--------: | --------- |
|  POST  | [/packages](changes/crud/packages/post.md)                                                 | Add a new package                                               |     -      | `package` |
| DELETE | [/packages/{name}](changes/crud/packages/name/delete.md)                                   | Remove a given package.                                         |     -      | -         |
|  POST  | [/classes](changes/crud/classes/post.md)                                                   | Define a new class or change the definition of an existing one. |     -      | `class`   |
| DELETE | [/classes/{name}](changes/crud/classes/name/delete.md)                                     | Remove a given class.                                           |     -      | -         |
|  POST  | [/classes/{name}/methods](changes/crud/classes/name/methods/post.md)                       | Compile a method in a given class.                              |     -      | `method`  |
| DELETE | [/classes/{name}/methods/{selector}](changes/crud/classes/name/methods/selector/delete.md) | Remove a given method.                                          |     -      | -         |

## [Changesets](changesets)

Endpoints to convert changesets (chunks file) to/from changes.

| Method | Path                                           | Description                                                 | Parameters |  Payload   |
| :----: | ---------------------------------------------- | ----------------------------------------------------------- | :--------: | :--------: |
|  POST  | [/changesets/download](changesets/download.md) | Convert a set of changes (JSON) into a chunk formatted-file |     -      | `[change]` |
|  POST  | [/changesets/upload](changesets/upload.md)     | Convert a chunk-formatted file into a set of changes (JSON) |     -      |  `string`  |

## [Evaluations](evaluations)

Endpoints to evaluate expressions and manage active evaluations.

| Method | Path                                              | Description                             | Parameters | Payload      |
| :----: | ------------------------------------------------- | --------------------------------------- | :--------: | ------------ |
|  POST  | [/evaluations](evaluations/post.md)               | Evaluate an expression                  |     -      | `evaluation` |
|  GET   | [/evaluations](evaluations/get.md)                | Retrieve active evaluations             |     -      | -            |
|  GET   | [/evaluations/id](evaluations/id/get.md)          | Retrieve the evaluation with a given ID |     -      | -            |
| DELETE | [/evaluations/id](evaluations/id/delete.md)       | Cancel the evaluation with a given ID   |     -      | -            |
|  POST  | [/evaluations/id/pause](evaluations/id/pause.md)  | Pause the evaluation with a given ID    |     -      | -            |
|  POST  | [/evaluations/id/resume](evaluations/id/resum.md) | Resume the evaluation with a given ID   |     -      | -            |

## [Objects](objects)

Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

| Method | Path                                        | Description                                    | Parameters | Payload      |
| :----: | ------------------------------------------- | ---------------------------------------------- | :--------: | ------------ |
|  GET   | [/objects](objects/get.md)                  | Retrieve pinned objects                        |     -      | -            |
|  GET   | [/objects/{id}](objects/id/get.md)          | Retrieve the pinned object with a given ID     |     -      | -            |
| DELETE | [/objects/{id}](objects/id/delete.md)       | Unpin the object with a given ID               |     -      | -            |
|  GET   | [/objects/{id}/\*](objects/id/slots/get.md) | Retrieve the object reached thru the given URI |     -      | -            |
|  POST  | [/objects](objects/post.md)                 | Pin the object reached thru the given URI      |     -      | `object URI` |
| DELETE | [/objects](objects/delete.md)               | Unpin all pinned objects                       |     -      | -            |

## [Workspaces](workspaces)

Endpoints to manage workspaces.

| Method | Path                                                       | Description                                        | Parameters | Payload |
| :----: | ---------------------------------------------------------- | -------------------------------------------------- | :--------: | ------- |
|  GET   | [/workspaces](workspaces/get.md)                           | Retrieve active workspaces                         |     -      | -       |
|  POST  | [/workspaces](workspaces/post.md)                          | Create a new workspace                             |     -      | -       |
|  GET   | [/workspaces/{id}](workspaces/id/get.md)                   | Retrieve the workspace with a given ID             |     -      | -       |
|  PUT   | [/workspaces/{id}](workspaces/id/put.md)                   | Update the workspace with a given ID               |     -      | -       |
| DELETE | [/workspaces/{id}](workspaces/id/delete.md)                | Delete the workspece with a given ID               |     -      | -       |
|  GET   | [/workspaces/{id}/bindings](workspaces/id/bindings/get.md) | Retrieve bindings of the workspece with a given ID |     -      | -       |

## [Debuggers](debuggers)

Endpoints to manage debuggers and interact with them.

| Method | Path                                                                                        | Description                                                                    | Parameters | Payload      |
| :----: | ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ | :--------: | ------------ |
|  GET   | [/debuggers](debuggers/get.md)                                                              | Retrieve open debuggers                                                        |     -      | -            |
|  POST  | [/debuggers](debuggers/post.md)                                                             | Create a debugger upon an given process (evaluation)                           |     -      | `evaluation` |
|  GET   | [/debuggers/{id}/frames](debuggers/id/frames/get.md)                                        | Retrieve frames of the debugger with a given ID                                |     -      | -            |
|  GET   | [/debuggers/{id}/frames/{index}](debuggers/id/frames/index/get.md)                          | Retrieve the _i_-th frame withing the debugger with a given ID                 |     -      | -            |
|  GET   | [/debuggers/{id}/frames/{index}/bindings](debuggers/id/frames/index/bindings/get.md)        | Retrieve the bindings of the _i_-th frame withing the debugger with a given ID |     -      | -            |
|  POST  | [/debuggers/{id}/frames/{index}/stepinto](debuggers/id/frames/index/stepinto/post.md)       | Step into the current sentence in the debugger with a given ID                 |     -      | -            |
|  POST  | [/debuggers/{id}/frames/{index}/stepover](debuggers/id/frames/index/stepover/post.md)       | Step over the current sentence in the debugger with a given ID                 |     -      | -            |
|  POST  | [/debuggers/{id}/frames/{index}/stepthrough](debuggers/id/frames/index/stepthrough/post.md) | Step through the current sentence in the debugger with a given ID              |     -      | -            |
|  POST  | [/debuggers/{id}/frames/{index}/runtocursor](debuggers/id/frames/index/runtocursor/post.md) | Run to cursor in the debugger with a given ID                                  |     -      | -            |
|  POST  | [/debuggers/{id}/frames/{index}/restart](debuggers/id/frames/index/restart/post.md)         | Restart the debugger with a given ID                                           |     -      | -            |
|  POST  | [/debuggers/{id}/resume](debuggers/id/resume/post.md)                                       | Resume the process of the debugger with a given ID                             |     -      | -            |
|  POST  | [/debuggers/{id}/terminate](debuggers/id/terminate/post.md)                                 | Terminate process being debugged and close the debugger with a given ID        |     -      | -            |
| DELETE | [/debuggers/{id}](debuggers/id/delete.md)                                                   | Closes the debugger with a given ID (terminating the process being debugged)   |     -      | -            |

## [Testing](test-runs)

Endpoints to run tests and retrieve their results.

| Method | Path                                                   | Description                                    | Parameters | Payload |
| :----: | ------------------------------------------------------ | ---------------------------------------------- | :--------: | ------- |
|  POST  | [/test-runs](test-runs/post.md)                        | Create and run of a test suite                 |     -      | `suite` |
|  GET   | [/test-runs/{id}](test-runs/id/get.md)                 | Retrieve the status of a given test suite run  |     -      | -       |
|  GET   | [/test-runs/{id}/results](test-runs/id/resutls/get.md) | Retrieve the restuls of a given test suite run |     -      | -       |
|  POST  | [/test-runs/{id}/run](test-runs/id/run/post.md)        | Re-run a given test suite                      |     -      | -       |
|  POST  | [/test-runs/{id}/debug](test-runs/id/debug/post.md)    | Debug a test withing a test suite run          |     -      | -       |
|  POST  | [/test-runs/{id}/stop](test-runs/id/stop/post.md)      | Stop an active test suite run                  |     -      | -       |
| DELETE | [/test-runs/{id}](test-runs/id/delete.md)              | Delete a test suite run                        |     -      | -       |

## [Profiling](profilers)

Endpoints to manage profilers and access their results.

| Method | Path                                                   | Description                                      | Parameters | Payload      |
| :----: | ------------------------------------------------------ | ------------------------------------------------ | :--------: | ------------ |
|  POST  | [/profilers](profilers/post.md)                        | Create a new profiler on a given expression      |     -      | `expression` |
|  GET   | [/profilers](profilers/get.md)                         | Retrieve active profilers                        |     -      | -            |
|  GET   | [/profilers/{id}](profilers/id/get.md)                 | Retrieve the profiler with a given ID            |     -      | -            |
|  GET   | [/profilers/{id}/tree](profilers/id/tree/get.md)       | Retrieve a tree-like results of a given profiler |     -      | -            |
|  GET   | [/profilers/{id}/ranking](profilers/id/ranking/get.md) | Retrieve ranked results of a given profiler      |     -      | -            |
| DELETE | [/profilers/{id}](profilers/id/delete.md)              | Delete a given profiler                          |     -      | -            |

## [Extensions](extensions)

Retrieve IDE extensions defined by the backend.

| Method | Path                             | Description                 | Parameters | Payload |
| :----: | -------------------------------- | --------------------------- | :--------: | ------- |
|  GET   | [/extensions](extensions/get.md) | Retrieve list of extensions |     -      | -       |

## [Commands](commands)

Retrieve definitions and process commands.

| Method | Path                                            | Description                  | Parameters | Payload |
| :----: | ----------------------------------------------- | ---------------------------- | :--------: | ------- |
|  GET   | [/command-definitions](commands/definitions.md) | Retrieve command definitions |     -      | -       |
|  POST  | [/commands](commands/post.md)                   | Process a command            | `command`  | -       |

## [Events](events)

This endpoint allows clients to subscribe to server-sent events.

| Method | Path                     | Description                     | Parameters | Payload |
| :----: | ------------------------ | ------------------------------- | :--------: | ------- |
|  GET   | [/events](events/get.md) | Subscribe to server-sent events |     -      | -       |
