# Debuggers
Endpoints to manage debuggers and interact with them.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/debuggers](post.md) | Create a debugger upon an given process (evaluation) | - | ```json { "process": "string" }``` |
| GET | [/debuggers/{id}/frames](frames/get.md) | Retrieve frames of the debugger with a given ID | - | - |
| GET | [/debuggers/{id}/frames/{index}](frames/index/get.md) | Retrieve the i-th frame withing the debugger with a given ID | - | - |
| GET | [/debuggers/{id}/frames/{index}/bindings](frames/index/bindings/get.md) | Retrieve the bindings of the i-th frame withing the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/stepover](/stepover/post.md) | Step over the current sentence in the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/stepinto](/stepinto/post.md) | Step into the current sentence in the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/restart](/restart/post.md) | Restart the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/resume](/resume/post.md) | Resume the process of the debugger with a given ID | - | - |
| POST | [/debuggers/{id}/terminate](/terminate/post.md) | Terminate process being debugged and closesthe debugger with a given ID | - | - |
| DELETE | [/debuggers/{id}](delete.md) | Closes the debugger with a given ID (terminating the process being debugged) | - | - |