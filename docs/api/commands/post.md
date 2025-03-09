# Command processing

Process a command.

**URL**: `/commands`

**Method**: `POST`

## Success Responses

**Code** : `200 OK`

**Payload**: `command` defined as:

```json
{
	"command": "string",
}
```
It can also contain more properties given by the parameters defined by the command ([see Command definitions](./definitions.md))

## Example 1: Pharo image cleanup

Taking the definition given in [Example 1: Pharo image cleanup](./definitions.md#example-1-pharo-image-cleanup), the payload the backend will receive should be like this:

```json
{
	"comand": "imageCleanup"
}
```

The Pharo implementation of the API, could be have some sort of command dispatcher in place based on the `command` property. For example:

```smalltalk
processCommand
	| command |
	command := self bodyAt: 'command'.
	(self respondsTo: command asSymbol)
		ifFalse: [^self badRequest: 'Invalid command'].
	self perform: command asSymbol.
	^ true
```

and the particular handler for `imageCleanUp`:

```smalltalk
imageCleanup

	Smalltalk cleanUp: false

```

_Note: this is just an example, implemented in Pharo 12, and may be not a good implementation as #cleanUp: has undesired effects on native UI (some progress bar)._

## Example 2: saving image with a different name

Considering the definition given at [Example 2: saving image with a different name](./definitions.md#example-2-saving-image-with-a-different-name), the command sent for processing should look like this:

```json
{
	"command": "saveImageAs",
	"imageName": "MyImage"
}
```

Again the backend could handle this command by doing something like: 

```smalltalk
saveImageAs

	| name |
	name := self bodyAt: 'name'.
	Smalltalk saveAs: name
```