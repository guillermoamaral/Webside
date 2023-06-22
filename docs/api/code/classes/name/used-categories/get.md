# Retrieve categories

Retrieve categories used throughout the hierarchy of a given class.

**URL**: `/class/{name}/used-categories`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[string]`

**Example:**: `Fraction` categories `GET /classes/Fraction/used-categories`.

```json
[
    "accessing",
    "changes support",
    "comparing",
    "constants",
    "converting",
    "copying",
    "date functions",
    "evaluating",
    "evaluation",
    "event handling",
    "file in/out",
    "functions",
    "gemstone",
    "header access",
    "help",
    "initialization",
    "inspecting",
    "instance behavior",
    "interrupts",
    "json",
    "logarithms",
    "lookup",
    "meta accessing",
    "perform",
    "remembering/restoring",
    "services",
    "ui",
    "unwinding",
]
```
