# Retrieve method history

Retrieve historical changes on a given method.

**URL**: `/classes/{name}/methods/{selector}/history`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[change]` where `change` can be one of the following:

```json
{
	"type": "AddMethod",
	"label": "string",
	"package": "string",
	"timestamp": "string",
	"author": "string",
	"sourceCode": "string",
	"changesSomething": "boolean",
	"canBeApplied": "boolean",
	"className": "string",
	"selector": "string",
	"category": "string"
}
```

Note that `type` is always `AddMethod` as these are the changes that can be retrieved from a given method.

**Example:**: retrieve history of `methodHistory` in `WebsideAPI`.

```json
[
    {
        "type": "AddMethod",
        "label": "WebsideAPI ≫ methodHistory",
        "package": "Webside Base",
        "timestamp": "2024-01-17T15:04:03.613-03:00",
        "author": "G Amaral",
        "sourceCode": "methodHistory\r\t| class selector method history |\r\tclass := self requestedClass.\r\tclass ifNil: [^self notFound].\r\tselector := self requestedSelector.\r\tselector ifNil: [^self notFound].\r\t(class includesSelector: selector) ifFalse: [^self notFound].\r\tmethod := class >> selector.\r\thistory := method project changes historyOf: method.\r\t^history collect: #asWebsideJson",
        "changesSomething": false,
        "canBeApplied": true,
        "className": "WebsideAPI",
        "selector": "methodHistory",
        "category": "code endpoints"
    },
    {
        "type": "AddMethod",
        "label": "WebsideAPI ≫ methodHistory",
        "package": "Webside Base",
        "timestamp": "2024-01-17T15:03:58.259-03:00",
        "author": "guille",
        "sourceCode": "methodHistory\r\t| class selector method history |\r\tclass := self requestedClass.\r\tclass ifNil: [^self notFound].\r\tselector := self requestedSelector.\r\tselector ifNil: [^self notFound].\r\tmethod := class >> selector.\r\thistory := method project changes historyOf: method.\r\t^history collect: #asWebsideJson",
        "changesSomething": true,
        "canBeApplied": true,
        "className": "WebsideAPI",
        "selector": "methodHistory",
        "category": "code endpoints"
    }
]
```
