# API
Webside is based on a Smalltalk system exposing the following API.
It's worth mentioning that this API is not "pure" REST at the moment of writing this documentation. However, extending it to be a REST-full API could bring some benefits to both Webside and other potential consumers interested in inspecting or controlling a Smalltalk system through HTTP. Of course, such a change would require upgrading Webside and this documentation accordingly.

# Sections
These are the different sections of the API. The target Smalltalk system must implement this API in order to use Webside upon it.

_Note: the segments in each case do not include the base URI. This will be prompted to the user at the moment of connecting Webside to the target Smalltalk._

* [Code](code): Endpoints to retrieve the code (projects, classes and methods).

* [Changes](changes): Endpoints to change code (projects, classes and methods).

* [Evaluations](evaluations): Endpoints to evaluate, debug and profile expressions.

* [Objects](objects): Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

* [Workspaces](workspaces): Endpoints to manage workspaces.

* [Debugging](debugging): Endpoints to manage debuggers and interact with them.

* [Processes](processes): Endpoints to manage active processes.

* [Tests](tests): Endpoints to run tests and retrieve their results.

* [Profiling](profiling): Endpoints to manage profilers and access their results.