# Retrieve a package

Retrieve extended methods of a given package.

**URL**: `/packages/{name}/methods`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[method]` as defined in (../../../methods/get.md):

**Example:**: `Webside` package `GET /packages/Webside/methods`.

```json
[
	{
		"selector": "asWebsideJson",
		"class": "RBVariableNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'value' put: name;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RPackageTag",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'name' put: name;\r\t\tat: 'classes' put: classNames;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "Dictionary",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| columns rows presentation |\r\tcolumns := OrderedCollection new.\r\tcolumns\r\t\tadd:\r\t\t\t(NeoJSONObject new\r\t\t\t\tat: 'field' put: 'key';\r\t\t\t\tat: 'label' put: 'Key';\r\t\t\t\tat: 'align' put: 'left';\r\t\t\t\tyourself);\r\t\tadd:\r\t\t\t(NeoJSONObject new\r\t\t\t\tat: 'field' put: 'value';\r\t\t\t\tat: 'label' put: 'Value';\r\t\t\t\tat: 'align' put: 'left';\r\t\t\t\tyourself).\r\trows := self associations\r\t\tcollect: [ :a | \r\t\t\tNeoJSONObject new\r\t\t\t\tat: 'key' put: a key asString ;\r\t\t\t\tat: 'value' put: a value asString;\r\t\t\t\tyourself ].\r\tpresentation := NeoJSONObject new\r\t\tat: 'type' put: 'table';\r\t\tat: 'title' put: 'Items';\r\t\tat: 'columns' put: columns;\r\t\tat: 'rows' put: rows;\r\t\tyourself.\r\t^ super asWebsideJson\r\t\tat: 'presentation' put: presentation;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/12/2022 03:48",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ NeoJSONObject new\r\t\tat: 'type' put: self class websideType asString;\r\t\tat: 'label' put: self printString;\r\t\tat: 'package' put: nil;\r\t\tat: 'timestamp' put: DateAndTime now asString;\r\t\tat: 'author' put: Author uniqueInstance fullName;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/16/2022 02:52",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBLiteralValueNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'value' put: value asString;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBCascadeNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson \r| children |\rchildren := messages collect: [ :n | n asWebsideJson  ].\r^super asWebsideJson at: 'children' put: children; yourself ",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBVariableRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tclass := json at: 'class' ifAbsent: nil.\r\tclass ifNotNil: [ class := self classObjectFor: class asSymbol ].\r\tvariableName := json at: 'variable' ifAbsent: nil",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBVariableRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'class' put: class name;\r\t\tat: 'variable' put: variableName;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBPullUpInstanceVariableRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tclass notNil\r\t\tifTrue: [ class := class superclass ]",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "Object",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ NeoJSONObject new\r\t\tat: 'class' put: self class name;\r\t\tat: 'indexable' put: self class isVariable;\r\t\tat: 'size' put: (self class isVariable ifTrue: [self size] ifFalse: [0]);\r\t\tat: 'printString' put: self printString;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBMethodNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| children |\r\tchildren := OrderedCollection with: selector asWebsideJson.\r\targuments do: [ :n | children add: n asWebsideJson  ].\r\tchildren add: body asWebsideJson .\r\t^super asWebsideJson at: 'children' put: children; yourself ",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBAddMethodChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\t| category |\r\tsuper fromWebsideJson: json.\r\tselector := json at: 'selector' ifAbsent: nil.\r\tselector ifNotNil: [ selector := selector asSymbol ].\r\tsource := json at: 'sourceCode' ifAbsent: nil.\r\tcategory := json at: 'category' ifAbsent: nil.\r\tcategory ifNil: [ category := Protocol unclassified ].\r\tself protocols: {category asSymbol}",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/14/2022 11:43",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBAddMethodChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| json |\r\tjson := super asWebsideJson.\r\tself source ifNotNil: [ :s | json at: 'sourceCode' put: s ].\r\t^ json\r\t\tat: 'selector' put: self selector;\r\t\tat: 'category' put: self protocol;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/13/2022 14:44",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBReturnNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson \r\t^super asWebsideJson at: 'children' put: { value asWebsideJson  }; yourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRemoveMethodChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tselector := json at: 'selector' ifAbsent: nil.\r\tselector ifNotNil: [selector := selector asSymbol]",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRemoveMethodChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^super asWebsideJson at: 'selector' put: selector; yourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBAddClassChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\t\r\tsuper fromWebsideJson: json.\r\tdefinition := json at: 'definition' ifAbsent: ''.\r\t(definition includesSubstring: 'category:')\r\t\tifFalse: [ definition := definition , ' category: #Undefined' ]",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRenameMethodRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\toldSelector := json at: 'selector' ifAbsent: nil.\r\toldSelector ifNotNil: [ oldSelector := oldSelector asSymbol.\r\t\t\tpermutation := 1 to: oldSelector numArgs ].\r\tnewSelector := json at: 'newSelector' ifAbsent: nil.\r\tnewSelector ifNotNil: [ newSelector := newSelector asSymbol ].\r",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRenameMethodRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^super asWebsideJson\r\tat: 'selector' put: oldSelector;\r\tat: 'newSelector' put: newSelector; yourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoryProtocolChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tprotocol := json at: 'category' ifAbsent: nil",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRefactoryProtocolChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'category' put: protocol;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "String",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tremoveKey: 'presentation' ifAbsent: [  ];\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/12/2022 12:20",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RPackage",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| methods |\r\tmethods := NeoJSONObject new.\r\tclassExtensionSelectors\r\t\tkeysAndValuesDo: [ :c :m | methods at: c asString put: m ].\r\tmetaclassExtensionSelectors\r\t\tkeysAndValuesDo: [ :c :m | methods at: c asString put: m ].\r\t^ super asWebsideJson\r\t\tat: 'name' put: name;\r\t\tat: 'classes' put: self definedClassNames;\r\t\tat: 'methods' put: methods;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/16/2022 13:18",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBMethodProtocolChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\t| method |\r\tsuper fromWebsideJson: json.\r\r\tsource\r\t\tifNil: [ method := self changeClass >> selector.\r\t\t\tmethod ifNotNil: [ source := method sourceCode ] ]",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/14/2022 11:50",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "Context",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson \r\t| interval |\r\tinterval := self pcRange.\r\tinterval := NeoJSONObject new at: 'start' put: interval first; at: 'end' put: interval last; yourself.\r\t^NeoJSONObject new at: 'label' put: self method printString;\r\tat: 'class' put: self receiver class asWebsideJson ;\r\tat: 'method' put: self method asWebsideJson ;\r\tat: 'interval' put: interval; yourself ",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "SyntaxErrorNotification",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| interval |\r\tinterval := NeoJSONObject new\r\t\tat: 'start' put: location;\r\t\tat: 'end' put: location; yourself.\r\t^ NeoJSONObject new\r\t\tat: 'description' put: self messageText;\r\t\tat: 'interval' put: interval;\r\t\tyourself\r\t\t",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBAssignmentNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson \r\t^super asWebsideJson at: 'children' put: { variable asWebsideJson . value asWebsideJson }",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "ClassDescription",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'name' put: self name;\r\t\tat: 'definition' put: self oldDefinition;\r\t\tat: 'superclass' put: (self superclass ifNotNil: [ :c | c name ]);\r\t\tat: 'comment' put: self comment;\r\t\tat: 'variable' put: self isVariable;\r\t\tat: 'package' put: self package name;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/16/2022 02:50",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBCommentChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tcomment := json at: 'comment' ifAbsent: ''",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBCommentChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^super asWebsideJson\r\t\tat: 'comment' put: comment; yourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoryChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRefactoryChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ NeoJSONObject new\r\t\tat: 'type' put: self class websideType asString;\r\t\tat: 'label' put: self changeString;\r\t\tat: 'package' put: nil;\r\t\tat: 'timestamp' put: DateAndTime now asString;\r\t\tat: 'author' put: Author uniqueInstance fullName;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/16/2022 02:52",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoryClassChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tclassName := json at: 'class' ifAbsent: nil.\r\tclassName ifNotNil: [ className := className asSymbol ].\r\tisMeta := className notNil and: [ className endsWith: ' class' ]\r\t",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRefactoryClassChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'class' put: className ;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "CompiledMethod",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ NeoJSONObject new\r\t\tat: 'selector' put: self selector;\r\t\tat: 'class' put: self methodClass name;\r\t\tat: 'category' put: self category;\r\t\tat: 'package' put: self package name;\r\t\tat: 'source' put: self sourceCode;\r\t\tat: 'author' put: self author;\r\t\tat: 'timestamp' put: self timeStamp;\r\t\tat: 'overriding' put: self isOverriding;\r\t\tat: 'overriden' put: self isOverridden;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/16/2022 02:58",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRenameVariableChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tclassName := json at: 'class' ifAbsent: nil.\r\tclassName ifNotNil: [ className := className asSymbol ].\r\tisMeta := className notNil and: [ className endsWith: ' class' ].\r\toldName  := json at: 'variable' ifAbsent: nil.\r\tnewName := json at: 'newName' ifAbsent: nil",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRenameVariableChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^super asWebsideJson\r\tat: 'class' put: className; at: 'variable'  put: oldName; at: 'newName'put: newName; yourself ",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBMessageNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| s children |\r\ts := NeoJSONObject new\r\t\tat: 'type' put: 'Selector';\r\t\tat: 'start' put: self start;\r\t\tat: 'end' put: self start + selector size;\r\t\tat: 'value' put: selector;\r\t\tyourself.\r\tchildren := OrderedCollection\r\t\twith: receiver asWebsideJson\r\t\twith: s.\r\targuments do: [ :n | children add: n asWebsideJson ].\r\t^ super asWebsideJson\r\t\tat: 'children' put: children;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBSequenceNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson \r\t| children |\r\tchildren := OrderedCollection new.\r\ttemporaries do: [ :n | children add: n asWebsideJson  ].\r\tstatements do: [ :n |  children add: n asWebsideJson].\r\t^super asWebsideJson at: 'children' put: children ; yourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRefactoryDefinitionChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^super asWebsideJson\r\t\tat: 'sourceCode' put: self definition;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBMethodRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tclass := json at: 'class' ifAbsent: nil.\r\tclass ifNotNil: [class := self classObjectFor: class asSymbol].",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBMethodRefactoring",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'class' put: class name;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "RBLiteralArrayNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson \r\t| children |\r\tchildren := contents collect: [ :n | n asWebsideJson  ].\r\t^super asWebsideJson at: 'children' put: children; yourself ",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RGMethodDefinition",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ self compiledMethod asWebsideJson ",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoryVariableChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\tvariable := json at: 'variable' ifAbsent: nil",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBRefactoryVariableChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t^ super asWebsideJson\r\t\tat: 'variable' put:  variable;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRenameClassChange",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\tsuper fromWebsideJson: json.\r\toldName := json at: 'class' ifAbsent: nil.\r\tnewName := json at: 'newName' ifAbsent: nil.",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^self class websideType",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": false
	},
	{
		"selector": "asWebsideJson",
		"class": "RBNode",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\r\t^ NeoJSONObject new\r\t\tat: 'type' put: self websideType;\r\t\tat: 'start' put: self start;\r\t\tat: 'end' put: self stop;\r\t\tyourself",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "asWebsideJson",
		"class": "Collection",
		"category": "*Webside",
		"package": "Webside",
		"source": "asWebsideJson\r\t| columns rows presentation |\r\tcolumns := OrderedCollection new.\r\tcolumns\r\t\tadd:\r\t\t\t(NeoJSONObject new\r\t\t\t\tat: 'field' put: 'index';\r\t\t\t\tat: 'label' put: '#';\r\t\t\t\tat: 'align' put: 'left';\r\t\t\t\tyourself);\r\t\tadd:\r\t\t\t(NeoJSONObject new\r\t\t\t\tat: 'field' put: 'value';\r\t\t\t\tat: 'label' put: 'Value';\r\t\t\t\tat: 'align' put: 'left';\r\t\t\t\tyourself).\r\trows := self asArray\r\t\twithIndexCollect: [ :e :i | \r\t\t\tNeoJSONObject new\r\t\t\t\tat: 'index' put: i;\r\t\t\t\tat: 'value' put: e asString;\r\t\t\t\tyourself ].\r\tpresentation := NeoJSONObject new\r\t\tat: 'type' put: 'table';\r\t\tat: 'title' put: 'Items';\r\t\tat: 'columns' put: columns;\r\t\tat: 'rows' put: rows;\r\t\tyourself.\r\t^ super asWebsideJson\r\t\tat: 'presentation' put: presentation;\r\t\tyourself",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/12/2022 03:46",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "websideType",
		"class": "RBRemoveProtocolChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'CategoryRemove'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBAddClassVariableChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'ClassVariableAddition'",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 5/29/2022 20:47",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBVariableNode class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType \r^'Identifier'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBMethodProtocolChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType \r\t^'MethodClassification'",
		"author": "guilleamaral",
		"timestamp": "guilleamaral 6/13/2022 14:17",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRemoveClassChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'ClassRemove'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRemoveInstanceVariableChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'InstanceVariableRemove'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRenameMethodRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'SelectorRename'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "acceptsWebsideJson:",
		"class": "RBReplaceMethodRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "acceptsWebsideJson: json\r\t| classname class method selector |\r\t(super acceptsWebsideJson: json)\r\t\tifFalse: [ ^ false ].\r\tclassname := json at: 'class' ifAbsent: [ ^ false ].\r\tclass := Smalltalk classNamed: classname.\r\tclass ifNil: [ ^ false ].\r\tmethod := RBParser parseMethod: (json at: 'sourceCode') onError: nil.\r\tmethod ifNil: [ ^ false ].\r\tselector := method selector.\r\tselector ifNil: [ ^ false ].\r\t^ class includesSelector: selector",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBAddMethodRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'MethodDefinition'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "acceptsWebsideJson:",
		"class": "RBAddMethodRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "acceptsWebsideJson: json\r\t| classname class method selector |\r\t(super acceptsWebsideJson: json)\r\t\tifFalse: [ ^ false ].\r\tclassname := json at: 'class' ifAbsent: [ ^ false ].\r\tclass := Smalltalk classNamed: classname.\r\tclass ifNil: [ ^ false ].\r\tmethod := RBParser parseMethod: (json at: 'sourceCode') onError: nil.\r\tmethod ifNil: [ ^ false ].\r\tselector := method selector.\r\tselector ifNil: [ ^ false ].\r\t^ (class includesSelector: selector) not",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRenameClassChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'ClassRename'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBAddMethodChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'MethodDefinition'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": true
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoryChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\t| class |\r\tclass := self classForWebsideJson: json.\r\t^ class ifNotNil: [ class new fromWebsideJson: json ] ",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRefactoryChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ nil",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "acceptsWebsideJson:",
		"class": "RBRefactoryChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "acceptsWebsideJson: json\r\t| type |\r\ttype := json at: 'type' ifAbsent: nil.\r\t^ self websideType = type",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": false
	},
	{
		"selector": "classForWebsideJson:",
		"class": "RBRefactoryChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "classForWebsideJson: json\r\t^ self allSubclasses\r\t\tdetect: [ :c | c acceptsWebsideJson: json ]\r\t\tifNone: nil",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBCommentChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'ClassCommentDefinition'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBNode class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType \r\t^(self name withoutPrefix: 'RB') withoutSuffix: 'Node'.",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "websideType",
		"class": "RBPushDownInstanceVariableRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'InstanceVariableMoveDown'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBAddInstanceVariableChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'InstanceVariableAddition'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBPullUpInstanceVariableRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType \r\t^'InstanceVariableMoveUp'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBAddClassChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'ClassDefinition'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "fromWebsideJson:",
		"class": "RBRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "fromWebsideJson: json\r\t| class |\r\tclass := self classForWebsideJson: json.\r\t^ class ifNotNil: [class new fromWebsideJson: json]",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ nil",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "acceptsWebsideJson:",
		"class": "RBRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "acceptsWebsideJson: json\r\t| type |\r\ttype := json at: 'type' ifAbsent: nil.\r\t^ self websideType = type",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": true
	},
	{
		"selector": "classForWebsideJson:",
		"class": "RBRefactoring class",
		"category": "*Webside",
		"package": "Webside",
		"source": "classForWebsideJson: json\r\t^ self allSubclasses\r\t\tdetect: [ :c | c acceptsWebsideJson: json ]\r\t\tifNone: nil",
		"author": "",
		"timestamp": "",
		"overriding": false,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRemoveMethodChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'MethodRemove'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	},
	{
		"selector": "websideType",
		"class": "RBRenameInstanceVariableChange class",
		"category": "*Webside",
		"package": "Webside",
		"source": "websideType\r\t^ 'InstanceVariableRename'",
		"author": "",
		"timestamp": "",
		"overriding": true,
		"overriden": false
	}
]
```
