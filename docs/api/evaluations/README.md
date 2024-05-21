# Evaluations

Endpoints to evaluate expressions and manage active evaluations.

| Method | Path                                  | Description                             | Parameters | Payload |              |
| :----: | ------------------------------------- | --------------------------------------- | :--------: | ------- | ------------ |
|  POST  | [/evaluations](post.md)               | Evaluate an expression                  |     -      | -       | `expression` |
|  GET   | [/evaluations](get.md)                | Retrieve active evaluations             |     -      | -       | -            |
|  GET   | [/evaluations/id](id/get.md)          | Retrieve the evaluation with a given ID |     -      | -       | -            |
| DELETE | [/evaluations/id](id/delete.md)       | Cancel the evaluation with a given ID   |     -      | -       | -            |
|  POST  | [/evaluations/id/pause](id/pause.md)  | Pause the evaluation with a given ID    |     -      | -       |
|  POST  | [/evaluations/id/resume](id/resum.md) | Resume the evaluation with a given ID   |     -      | -       |

## About evaluations and processes

The term _evaluation_ in the context of this API is rather vague as it encompasses both an expression issued for its evaluation and the actual process of such evaluation.
It is likely that the target Smalltalk system implements this by means of some sort of _process_.
In this sense, an evaluation can suspended and resumed, or even terminated, much as its corresponding process in the back-end can.
See [/evaluations/id/pause](id/pause.md) and [/evaluations/id/resume](id/resume.md) for an example on pausing, debugging and resuming an evaluation.
