# Profilers

A profiler is just a process observing another with the purpose of gathering statistics about executed code.\
In this sense, the API proposes to treat a profiler as an asynchronous evaluation (see [\evaluations](../evaluations/post.md#asyncrhonous-evaluations) for more information).\
The proposed workflow is the following:

1. Issue a profiler, either by a `POST` to `/profilers`, or by using `profile=true` on a `POST` to `/evaluations` (see [ evaluations](../evaluations/post.md)).
2. Follow the progress of the profiler by polling `/profilers/{id}` with profiler's ID (much as an evaluation progress is followed)
3. Once the profiler state is `finished`, results can be retrieved from `/profilers/{id}/tree` and `/profilers/{id}/ranking`.

Endpoints to manage profilers and access their results.

| Method | Path                                         | Description                                      | Parameters | Payload      |
| :----: | -------------------------------------------- | ------------------------------------------------ | :--------: | ------------ |
|  POST  | [/profilers](post.md)                        | Create a new profiler on a given expression      |     -      | `expression` |
|  GET   | [/profilers](get.md)                         | Retrieve active profilers                        |     -      | -            |
|  GET   | [/profilers/{id}](id/get.md)                 | Retrieve the profiler with a given ID            |     -      | -            |
|  GET   | [/profilers/{id}/tree](id/tree/get.md)       | Retrieve a tree-like results of a given profiler |     -      | -            |
|  GET   | [/profilers/{id}/ranking](id/ranking/get.md) | Retrieve ranked results of a given profiler      |     -      | -            |
| DELETE | [/profilers/{id}](id/delete.md)              | Delete a given profiler                          |     -      | -            |
