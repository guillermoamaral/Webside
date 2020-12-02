# Debuggers
Endpoints to manage debuggers and interact with them.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| POST | [/debuggers](debuggers/post.md) | Create a debugger upon an given process (evaluation) | - | | ```json { "process": "string" }``` |
| GET | [/debuggers/{id}/frames](debuggers/frames/get.md) | Retrieve frames of the debugger with a given ID | - | | - |
| GET | [/debuggers/{id}/frames/{index}](debuggers/frame/get.md) | Retrieve the i-th frame withing the debugger with a given ID | - | | - |
| GET | [/debuggers/{id}/frames/{index}/bindings](debuggers/frame/bindings/get.md) | Retrieve the bindings of the i-th frame withing the debugger with a given ID | - | | - |
| POST | [/debuggers/{id}/skip](debuggers/skip.md) | Skip the current sentence in the debugger with a given ID | - | | - |
| POST | [/debuggers/{id}/hop](debuggers/hop.md) | Goes into the current sentence in the debugger with a given ID | - | | - |
| POST | [/debuggers/{id}/restart](debuggers/restart.md) | Restart the debugger with a given ID | - | | - |
| POST | [/debuggers/{id}/resume](debuggers/resume.md) | Resume the process of the debugger with a given ID | - | | - |
| POST | [/debuggers/{id}/terminate](debuggers/terminate.md) | Terminate process being debugged and closesthe debugger with a given ID | - | | - |
| DELETE | [/debuggers/{id}](debuggers/delete.md) | Closes the debugger with a given ID (terminating the process being debugged) | - | | - |