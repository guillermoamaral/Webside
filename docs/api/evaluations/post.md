# Evaluate an expression

This endpoint allows to evaluate an expression, either **synchronous-** or **asynchronously**.

*Note: Besides this dichotomy from the API consumer standpoint (the IDE for instance), the target Smalltalk system can manage this in different ways. Most likely, a Smalltalk process for evaluating the given expression is going to represent what we call an *evaluation* here.*

**URL**: `/evaluations`

**Method**: `POST`

**Payload**:

```json
{
	"expression": "string",
	"context": "context",
	"sync": "boolean",
	"pin": "boolean",
	"debug": "boolean",
	"profile": "boolean"
}
```

Where

-   `sync` indicates whether

    -   (`sync=true`) the request is blocked until the evaluation finishes, and the response contains the object resulting from the evaluation, or,
    -   (`sync=false`) the request is immediately responded with the ID of the new evaluation, so the client can follow its progress (see below). This mode allows the IDE to have control over the evaluation, and offer the user the chance to interrupt it (for debugging), or even terminate it.

-   `pin` makes the resulting object to be either pinned (i.e., make sure the backend keeps a reference to the object, with the same ID as the evaluation that generated it), or discarded.\
    Note that an asynchronous evaluations will forcely pin the resulting object. Otherwise, it might be collected by the GC before the client requests it.

-   `debug` forces the immediate suspension of the evaluation and creates a debugger on it, returing the ID of the created debugger.

-   `profile` creates a profiler and returns the ID of the created profiler to follow its progress and get its results when it finishes.

-   `context` can be one of the following:

    ```json
    {
    	"class": "string"
    }
    ```

    ```json
    {
    	"object": "string"
    }
    ```

    ```json
    {
    	"workspace": "string"
    }
    ```

    ```json
    {
    	"debugger": "string",
    	"frame": "number"
    }
    ```

Where,

-   `class` is the name of a class.
-   `object` is the ID of a pinned object.
-   `workspace` is the ID of an existing workspace.
-   `debugger` is the ID of an existing debugger and `frame` is the index of the frame within its current frames.

# Syncrhonous evaluations

In case of a synchronous evaluation the request will be blocked until the evaluation finishes and the result will be the same as retrieving any pinned object (see [`/objects/{id}`](../objects/id/get.md)).

**Example:**: evaluate `3 + 4` synchronously, without pinning the resulting object, with no context:\

`POST /evaluations`
```json
{
	"expression": "3 + 4",
	"sync": true,
	"pin": false
}
```

And the result:

```json
{
	"class": "SmallInteger",
	"hasNamedSlots": false,
	"hasIndexedSlots": false,
	"size": 0,
	"printString": "7"
}
```

## Errors

In case of an evaluation error, the response of a synchronous evaluation should have the code `500` and the payload should include the following information:

```json
{
	"description": "string",
	"interval": {
        "start": "number",
        "end": "number"
    }
	"evaluation": "string",
	"stack": "string"
}
```

Where,

-   `description` is a description of the error.
-   `interval` (optional) the expression interval where the error was encountered.
-   `evaluation` is the ID of the evaluation and it serves to create a debugger on the evaluation process.
-   `stack` (optional) is a string contaning the stack trace (only frame headers or labels) as separated lines (this could be used as a snapshot for the user before entering in an actual debugger).

For example, the following error is returned after trying to evaluate `1 + `:

```json
500
{

    "description": "Variable or expression expected",
    "interval": {
        "start": 5,
        "end": 5
    },
    "evaluation": "1"
}
```

### Suggestions

There might be cases where the backend is able to suggest some ways to overcome a given evaluation error. For instance, lets suppose
we send the following evaluation request:

```json
{
	"expression": "NonExistingGlobal := 1"
}
```

Lets suppose that `NonExistingGlobal` does not exist in the system but `ExistingGlobal1` and `ExistingGlobal2` do. The backend could return the following suggestions:

```json
{
	"description": "undeclared NonExistingGlobal",
	"evaluation": "2",
	"stack": "SmalltalkCompiler>>undeclared...",
	"suggestions": [
		{
			"description": "Declare 'NonExistingGlobal' as a global",
			"changes": [
				{
					"type": "ExpressionEvaluation",
					"expression": "Smalltalk at: #'NonExistingGlobal' asSymbol put: nil."
				}
			],
			"expression": "NonExistingGlobal := 1"
		},
		{
			"description": "Did you mean ExistingGlobal1?",
			"changes": [],
			"expression": "ExistingGlobal1 := 1"
		},
		{
			"description": "Did you mean ExistingGlobal2?",
			"changes": [],
			"expression": "ExistingGlobal2 := 1"
		}
	]
}
```

Note that there is a list of `suggestions`, where each of them might contain some `changes` to be executed prior the expression evaluation, and an `expression` that can be different from the original one.
For instance, the first suggestion is to define a new global variable and so it contains a change to accomplish that (the `EvaluationExpression`), and the same `expression` as the original.
The second and third suggestions are to use different globals, taken from a list of existing globals, similar to the original one (`ExistingGlobal1` and `ExistingGlobal2`).

# Asyncrhonous evaluations

In this case, an immediate response with the ID of the new evaluation is returned. The client can follow the progress of the evaluation by polling the backend using such ID.

## Success Responses

**Code** : `201 Created`

**Payload**: the response of an asynchronous evaluation looks like this:

```json
{
	"id": "string",
	"state": "string"
}
```

Where

-   `state` is any of these: `pending`, `evaluating`, `paused`, `failed`, `cancelled` or `finished`. The first state should be `pending`.

**Example:**: evaluate `3 + 4` asynchronously:\

`POST /evaluations`
```json
{
	"expression": "3 + 4",
	"sync": true,
	"pin": false
}
```

And the result:

```json
{
	"id": "1",
	"state": "pending"
}
```

## Following evaluation progress

With this ID, the client can send a request to see how the evaluation goes (`GET /evaluations/1` in the example) to obtain a response like this:

```json
{
	"id": "1",
	"state": "finished"
}
```

Alternatively, there could be several checks with either `pending` or `evaluating` states. That will depend on when the backend starts the evaluation and how long it takes to it to finish.

## Errors

As in the case of synchronous evaluation, asynchronous ones might result in errors. However, in this case, the error will be reported in a regular state response (and not through an HTTP error code), by means of `failed` state, and an additional property `error` with a description plus potential suggestions as described for [synchronous evaluation errors](#errors).

Lets suppose the evaluation request contains the expresion `3 + `. The first response will correspond to the evaluation creation:

```json
{
	"id": "2",
	"expression": "3 + ",
	"state": "pending"
}
```

Then, a subsequent request to see the state of this evaluation will return:

```json
{
	"id": "2",
	"expression": "3 + ",
	"state": "failed",
	"error": {
		"description": "Variable or expression expected",
		"interval": {
			"start": 5,
			"end": 5
		}
	}
}
```

Here we see the evaluation has `failed` together with the occurred error.

From here, it is possible to create a debugger by using the evaluation ID (see [/debuggers](../debuggers/post.md))
