# Changes

These endpoints are used to apply changes and retrieve changes made to the system.

**Important**. The API specifies a set of _basic_ changes applicable to all dialect analized. However, this list can be extended to support more changes and refactorings by means of [/extensions](../extensions/ChangeExtensions.md) endpoint.

| Method | Path                                                                                               | Description                                   | Parameters | Payload  |
| :----: | -------------------------------------------------------------------------------------------------- | --------------------------------------------- | :--------: | -------- |
|  GET   | [/changes](get.md)                                                                                 | Retrieve changes made to the system           |   author   | -        |
|  POST  | [/changes](post.md)                                                                                | Apply a change to the system                  |     -      | `change` |
|  GET   | [/classes/{name}/methods/{selector}/history](../code/classes/name/methods/selector/history/get.md) | Retrieve historical changes on a given method |     -      | -        |
