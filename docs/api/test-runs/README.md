# Testing

Endpoints to run tests and retrieve their results.

| Method | Path                                         | Description                                    | Parameters | Payload                                                                      |
| :----: | -------------------------------------------- | ---------------------------------------------- | :--------: | ---------------------------------------------------------------------------- |
|  POST  | [/test-runs](post.md)                        | Create and run of a test suite                 |     -      | `json { "methods": ["string"], "classes": ["string"], "package": "string" }` |
|  GET   | [/test-runs/{id}](id/get.md)                 | Retrieve the status of a given test suite run  |     -      | -                                                                            |
|  GET   | [/test-runs/{id}/results](id/resutls/get.md) | Retrieve the restuls of a given test suite run |     -      | -                                                                            |
|  POST  | [/test-runs/{id}/run](id/run/post.md)        | Re-run a given test suite                      |     -      | -                                                                            |
|  POST  | [/test-runs/{id}/debug](id/debug/post.md)    | Debug a test withing a test suite run          |     -      | -                                                                            |
|  POST  | [/test-runs/{id}/stop](id/stop/post.md)      | Stop an active test suite run                  |     -      | -                                                                            |
| DELETE | [/test-runs/{id}](id/delete.md)              | Delete a test suite run                        |     -      | -                                                                            |
