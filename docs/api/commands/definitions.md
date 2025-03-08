# Command definitions

A _command_ is any action defined by the backend and that can be exposed by the IDE, allowing the user to trigger such action.

**URL**: `/command-definitions`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[commandDefinition]` where `commandDefinition` is defined as:

```json
{
	"name": "string",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"parameters": ["parameter1", "parameter2"],
	"section": "string",
	"needsConfirmation": "boolean"
}
```

Where:
-   `name` the name of the command used when submitting the command.
-   `elementType` represents the meta-model object over which the command is applied. It can be `system`, `package`, `class`, `variable`, `category`, `method` or `code`.
-   `label` a text that will be used by the IDE to present the command as an option.
-   `description` (optional) a description of the command (it might be used as a tip text or help).
-   `parameters` zero or more parameters that will be prompted to the user and that will be sent to the backend to process the command (see [below](#parameters)).
-   `section` (optional) it could be used by the IDE to place the option under a submenu. By default no submenu is used (i.e., the option will be appended to the corresponding menu).
-   `needsConfirmation` (optional) specifies whether the command should be confirmed by the user. Default is `false.`.

## Parameters

As said above, `parameters` will be prompted to the user and sent to the backend for processing the command.

Their structure should be like this:

```json
{
	"name": "string",
	"label": "string",
	"defaultValue": "value",
	"type": "string",
	"options": "string"
}
```

Where,

-   `name` is the name of the parameter and it will be used as a property of the command payload sent to the backend.
-   `label` will be used to prompt the user for a value.
-   `defaultValue` (optional) is the default value presented to the user. In the case of a string, it can contain zero or more attribute expressions of the form `element.xxxx` (see below).
-   `type` specifies how the parameter will be prompted to the user. At the moment of writing this document it can be `text`, `number` or `boolean`.
-   `options` (optional) provides a list of alternatives. This can be either a fixed list, or a _dynamic_ one. By the moment, there are only two possibilities for the latter: `packages` or `classes`, to provide the user with the list of the names of existing packages and classes, respectively.

### Attributes expressions

If the default value of a parameter contains `{element.xxxx}`, the actual value will be extracted from the meta-model object (`element`) from which the command is triggered (most likely the object selected in IDE), and the attribute given by the string after the dot (`xxxx`).

Valid attributes can be extracted from the corresponding endpoints in the documentation ([pacakges](../code/packages/get.md), [classes](../code/classes/get.md), [variables](../code/classes/name/variables/get.md), [categories](../code/classes/name/categories/get.md), [methods](../code/methods/get.md)).

Note that an element attribute can be _immersed_ in a wider string. Moreover, there could be more than one attribute.\
As a general rule, the default value will be the result of replacing every "dot" expression (expression between curly brakets), leaving the rest of the string as it is. For example, the actual value of a property whose value is specified as `{element.methodClass} >> #{element.selector}`, applicable on a `method` element whose `methodClass` is `Point` and its `selector` is `x`, will be `Point >> x`.

## Example 1: Pharo image cleanup

Lets suppose we are targeting Pharo and that we want to expose the option _Do image cleanup_. The definition should look like:

```json
{
	"type": "command",
	"name": "imageCleanup",
	"elementType": "system",
	"label": "Do image cleanup",
	"parameters": [],
	"needsConfirmation": true
}
```

The IDE should show this command as an option in the system menu (as the `elementType` is `system`), and when the user hits this option, it should send the command to the backend for proceesing (see [processing commands](post.md)).

Note that this command does not require any parameter and it does require confirmation. The IDE will likely ask the user for confirmation.

## Example 2: saving image with a different name

Now consider a command for saving the image but with a name provided by the user.

```json
{
	"type": "command",
	"name": "saveImageAs",
	"elementType": "system",
	"label": "Save image as",
	"parameters": [
		{
			"name": "imageName",
			"label": "Image name",
			"defaultValue": "MyImage",
			"type": "text"
		}
	]
}
```

Again this is a system command and so the value of `elementType`. However, this commands requires a parameter, `imageName`.

With this definition, users will find an option _Save image as_ withing the system menu options, and when they hit it, they will be prompted for an _Image name_. Should the user accept the prompt with `MySuperCoolImage` as the name, the IDE will send the command for processing.

```json
{
	"command": "saveImageAs",
	"imageName": "MySuperCoolImage"
}
```
