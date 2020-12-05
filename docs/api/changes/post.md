# Retrieve classes
Apply a change to the system.

**URL**: `/changes`

**Method**: `POST`

**Body**: one of the following:

```json
{
    "type": "MethodDefinition",
    "class": "string",
    "category": "string",
    "sourceCode": "string",
    "author": "string"
}
```

```json
{
    "type": "MethodRemove",
    "class": "string",
    "selector": "string"
}
```

```json
{
    "type": "SelectorRename",
    "class": "string",
    "selector": "string",
    "newSelector": "string"
}
```

```json
{
    "type": "ClassDefinition",
    "class": "string",
    "definition": "string"
}
```

```json
{
    "type": "ClassCommentDefinition",
    "class": "string",
    "comment": "string"
}
```

```json
{
    "type": "ClassRemove",
    "class": "string"
}
```

```json
{
    "type": "ClassRename",
    "class": "string",
    "newName": "string",
    "renameReferences": "boolean"
}
```

```json
{
    "type": "InstanceVariableAddition",
    "class": "string",
    "variable": "string"
}
```

```json
{
    "type": "InstanceVariableRename",
    "class": "string",
    "variable": "string",
    "newName": "string"
}
```

```json
{
    "type": "InstanceVariableRemove",
    "class": "string",
    "variable": "string"
}
```

```json
{
    "type": "InstanceVariableMoveUp",
    "class": "string",
    "variable": "string"
}
```

```json
{
    "type": "InstanceVariableMoveDown",
    "class": "string",
    "variable": "string",
    "target": "string",
}
```

```json
{
    "type": "CategoryRename",
    "class": "string",
    "category": "string",
    "newName": "string",
}
```

```json
{
    "type": "CategoryRemove",
    "class": "string",
    "category": "string"
}
```

## Success Responses

**Code** : `200 OK`

**Content**: the `change` sent in the body

**Example:**: compile method `phi` in `Float`
`POST /changes`

```json
{
    "type": "MethodDefinition",
    "class": "Float class",
    "category": "constants",
    "sourceCode": "phi\r\t^1.0 + 5.0 sqrt / 2.0",
    "author": "guille"
}
```