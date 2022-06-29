# Debuggers

Endpoints to manage debuggers and interact with them.

| Method | Path                                                                       | Description                                                                    | Parameters | Payload                      |
| :----: | -------------------------------------------------------------------------- | ------------------------------------------------------------------------------ | :--------: | ---------------------------- |
|  GET   | [/debuggers](get.md)                                                       | Retrieve open debuggers                                                        |     -      | -                            |
|  POST  | [/debuggers](post.md)                                                      | Create a debugger upon an given process (evaluation)                           |     -      | `{ "evaluation": "string" }` |
|  GET   | [/debuggers/{id}/frames](id/frames/get.md)                                 | Retrieve frames of the debugger with a given ID                                |     -      | -                            |
|  GET   | [/debuggers/{id}/frames/{index}](id/frames/index/get.md)                   | Retrieve the _i_-th frame withing the debugger with a given ID                 |     -      | -                            |
|  GET   | [/debuggers/{id}/frames/{index}/bindings](id/frames/index/bindings/get.md) | Retrieve the bindings of the \_i-th frame withing the debugger with a given ID |     -      | -                            |
|  POST  | [/debuggers/{id}/stepover](id/stepover/post.md)                            | Step over the current sentence in the debugger with a given ID                 |     -      | -                            |
|  POST  | [/debuggers/{id}/stepinto](id/stepinto/post.md)                            | Step into the current sentence in the debugger with a given ID                 |     -      | -                            |
|  POST  | [/debuggers/{id}/restart](id/restart/post.md)                              | Restart the debugger with a given ID                                           |     -      | -                            |
|  POST  | [/debuggers/{id}/resume](id/resume/post.md)                                | Resume the process of the debugger with a given ID                             |     -      | -                            |
|  POST  | [/debuggers/{id}/terminate](id/terminate/post.md)                          | Terminate process being debugged and closesthe debugger with a given ID        |     -      | -                            |
| DELETE | [/debuggers/{id}](id/delete.md)                                            | Closes the debugger with a given ID (terminating the process being debugged)   |     -      | -                            |
