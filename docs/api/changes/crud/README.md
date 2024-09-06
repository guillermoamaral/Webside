# Basic CRUD operations

For the sake of simplicity, the API encourages the use of a single endpoint [`/changes`](../../changes/README.md) for applying any change to the system.\
However, as this may imply to count on some implementation of _refactoring changes_ on the backend (and might not be the case),
a basic set of CRUD operations on main meta-model objects (classes and methods), should be provided.
The data required for these operations should be the same as the one provided for the corresponding changes.

| Method | Path                                                                          | Description                                                     | Parameters | Payload   |
| :----: | ----------------------------------------------------------------------------- | --------------------------------------------------------------- | :--------: | --------- |
|  POST  | [/packages](packages/post.md)                                                 | Add a new package                                               |     -      | `package` |
| DELETE | [/packages/{name}](packages/name/delete.md)                                   | Remove a given package.                                         |     -      | -         |
|  POST  | [/classes](classes/post.md)                                                   | Define a new class or change the definition of an existing one. |     -      | `class`   |
| DELETE | [/classes/{name}](classes/name/delete.md)                                     | Remove a given class.                                           |     -      | -         |
|  POST  | [/classes/{name}/methods](classes/name/methods/post.md)                       | Compile a method in a given class.                              |     -      | `method`  |
| DELETE | [/classes/{name}/methods/{selector}](classes/name/methods/selector/delete.md) | Remove a given method.                                          |     -      | -         |

[^1]: Property `definition` is highly dependent on the backend and can be substituted by properties `instanceVariables`, `classVariables` and `poolDictionaries`.
