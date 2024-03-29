# Evaluate an expression

Evaluate an expression, either synchronously or asynchronously.

Special attention must be paid to the way compilation errors should be handled by the API so the front-end can react properly.

**URL**: `/evaluations`

**Method**: `POST`

**Body**: an `expression` plus the following properties:

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
- `sync` indicates whether the call should be blocked until the evaluation finishes or return immediately (with the ID of the just created evaluation to follow its state). 
Asynchronous evaluations allow the IDE to have control over the evaluation, being able to offer the user the chance to cancel the evaluation or interrupt it and debug it (a feature that is not implemented at the moment of writing this documentation).  
Once Webside has the evaluation ID, it can make a synchronous call to [`/objects/{id}`](../objects/id/get.md) (with the ID of the evaluation) to avoid polling the state of the evaluation before getting the object.

- `pin` makes the resulting object to be pinned (with the same ID of the evaluation that generated it) or be discarded. Of course, an asyncrhonic evaluation will keep (pin) the object once the evaluation finishes so the client is able to get it.

- `debug` inserts a breakpoint right before the expression and triggers a synchronous evaluation (as it has a breakpoint it should return immediately), returing the ID of a debugger created on the active evaluation (process).

- `profile` creates a profiler and forces a synchronous profiling of the expression at hand and then returns the ID of the created profiler to fetch their results.

- `context` can be one of the following:

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
Where
- `class` is the name of a class.
- `object` is the ID of a pinned object.
- `workspace` is the ID of an existing workspace.
- `debugger` is the ID of an existing debugger and `frame` is the index of the frame within its current frames.

## Success Responses

**Code** : `200 OK`

**Content**: in case of an asynchronous evaluation the API should return:

```json
{
	"id": "string",
	"expression": "string",
	"state": "string"
}
```

Where `state` is either `pending` or `finished`.

In case of a synchronous evaluation the result is the same as the one of [`/objects/{id}`](../objects/id/get.md).

**Example:**: evaluate `3 + 4` synchronously, without pinning the resulting object, with no context:
`POST /evaluations`

```json
{
	"expression": "2 + 4",
	"sync": true,
	"pin": false
}
```

And this is the result:

```json
{
	"class": "SmallInteger",
	"hasNamedSlots": false,
	"hasIndexedSlots": false,
	"size": 0,
	"printString": "6"
}
```

### Error Details

In case of an evaluation error, be it as the response of a synchronous evaluation or as the result of trying the get the resulting object after an asynchronous evaluation, the server should respond with code `500` and a payload with the following aspect:

```json
{
	"description": "string",
	"evaluation": "string",
	"stack": "string"
}
```

Here, `description` is the error text describing what went wrong.  
`evaluation` is the ID of the evaluation created and it serves as the parameter to create a debugger on the evaluation process.  
`stack` is a string contaning the stack trace (only frame headers or labels) as separated lines (this is meant to be used as a snapshot for the user before entering in an actual debugger).

For example, the following error is returned after trying to evaluate `1 + `:

```json
500
{
	"description": "primary missing",
	"evaluation": "1",
	"stack": "SmalltalkCompiler>>error:stretch:\rSmalltalkCompiler>>error: 'primary missing' at: 3 \rSmalltalkParser>>error: 'primary missing' at: 3 \rSmalltalkParser>>error: 'primary missing' \rSmalltalkParser>>binaryMessage:\rSmalltalkParser>>binarySequence:\rSmalltalkParser>>expression\rSmalltalkParser>>statement\rSmalltalkParser>>statements\rSmalltalkParser>>addStatementsTo:\rSmalltalkParser>>addBodyTo:\rSmalltalkParser>>headlessMethod\rSmalltalkCompiler>>parseExpression\r[] in SmalltalkCompiler>>compileExpression:\rObject(BlockClosure)>>setUnwind:\rBlockClosure>>ensure:\rProcess>>useExceptionHandler:while:\rBlockClosure>>on:do:\rCompiler>>protect:\rSmalltalkCompiler>>compileExpression: '1 +' \rCompiler>>evaluate: '1 +' for: nil \r[] in Compiler>>evaluate:for:ifFail:\rObject(BlockClosure)>>setUnwind:\rBlockClosure>>ensure:\rProcess>>useExceptionHandler:while:\rBlockClosure>>on:do:\rCompiler>>evaluate: '1 +' for: nil ifFail: nil \r[] in WebsideEvaluation>>evaluate\r[] in WebsideEvaluation>>evaluateBlock:\rMessageSend(Message)>>performOn:\rMessageSend>>perform\rMessageSend>>evaluate\rProcess>>privatePerform:\rProcess>>basicEvaluate:\rMessageSend>>newProcess\r"
}
```

#### Suggestions
There might be other special cases where the back-end could suggest some ways to overcome a given evaluation error. For instance, suppose
the following evaluation request:

```json
{
	"expression": "NonExistingGlobal := 1",
}
```
Assuming that `NonExistingGlobal` does not actually exist in the system, the back-end could return suggestions in the followin way:

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
                    "author": "guille",
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

Note that there is a list of `suggestions`, where each of them might contain some `changes` to be executed prior the expression evaluation, and this `expression` can be potentially different from the original one.
For instance, the first suggestion is to define a new global variable and so it contains a change to accomplish that (the `EvaluationExpression`), and the same `expression` as the original.
The second and third suggestions are to use different globals, taken from a list of existing globals, similar to the original one (`ExistingGlobal1` and `ExistingGlobal2`).
