# API
These are the different sections of the API required by Webside. That is, the "host" Smalltalk image must implement this API for Webside to work properly.

_Note: paths are documented without a base URI_


[Code](code): Endpoints to retrieve the code (projects, classes and methods).

[Changes](changes): Endpoints to change code (projects, classes and methods).

[Evaluations](evaluations): Endpoints to evaluate, debug and profile expressions.

[Objects](objects): Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

[Workspaces](workspaces): Endpoints to manage workspaces.

[Debugging](debugging): Endpoints to manage debuggers and interact with them.

[Processes](processes): Endpoints to manage active processes.

[Tests](tests): Endpoints to run tests and retrieve their results.

[Profiling](profiling): Endpoints to manage profilers and access their results.