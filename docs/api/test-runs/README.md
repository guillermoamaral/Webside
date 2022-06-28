# Testing

Endpoints to run tests and retrieve their results.

| Method | Path                                         | Description                              | Parameters | Payload                                                                         |
| :----: | -------------------------------------------- | ---------------------------------------- | :--------: | ------------------------------------------------------------------------------- |
|  POST  | [/test-runs](post.md)                        | Create and run of a test suite           |     -      | `json { "methods": ["string"], "classes": ["string"], "packages": ["string"] }` |
|  GET   | [/test-runs/{id}/status](id/status/get.md)   | Retrieve the status of a given test run  |     -      | -                                                                               |
|  GET   | [/test-runs/{id}/results](id/resutls/get.md) | Retrieve the restuls of a given test run |     -      | -                                                                               |
|  POST  | [/test-runs/{id}/run](id/run/post.md)        | Re-run a given test suite                |     -      | -                                                                               |
|  POST  | [/test-runs/{id}/debug](id/debug/post.md)    | Debug a test withing a test run          |     -      | -                                                                               |
|  POST  | [/test-runs/{id}/stop](id/stop/post.md)      | Stop an active test run                  |     -      | -                                                                               |
| DELETE | [/test-runs/{id}](id/delete.md)              | Delete a test run                        |     -      | -                                                                               |
