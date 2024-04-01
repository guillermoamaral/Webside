# Change extensions

This type of extension allows us to extend the set of changes that the API specifies (and that the IDE will expect to be present). Hence, specifications retrieved from this endpoint will enlarge the list of options available on the IDE.

Of course, every change defined here must be supported by the system. At the moment of writing this documentation it is the same system that both **extends** the list of changes and **processes** them. But in the future it could be extended in a different way. In any case, if a change `MySuperCoolChange` is defined, the target system must accept it as a valid change afterwards.

## Target object

First of all, a change _refers_ or is applicable over a given meta-model object. Namely, a package, a class, a variable, a category, or a method (plus a special element named _code_). Each of these elements has its corresponding `elementType` here: `package`, `class`, and so on.

# Specification

The structure of a change specification must have this structure:

```json
{
	"extensionType": "string",
	"elementType": "string",
	"label": "string",
	"description": "string",
	"properties": {
		"property1": "value1",
		"property2": "value2"
	},
	"parameters": ["parameter1", "parameter2"],
	"section": "string",
	"needsConfirmation": "boolean"
}
```

Where:

-   `extensionType` as these are extensions aimed at extending the built-in set of changes, this property is `change`.
-   `elementType` represents the meta-model object over which the change applies. It can be `package`, `class`, `variable`, `category`, `method` or `code`.
-   `label` a text that will be used by the IDE to present the option.
-   `description` (optional) a description of the change (it might be used as a tip text or help).
-   `properties` contains the actual properties of the change to be posted.
-   `parameters` (optional) describe what would be prompted to the user to fill some of the change properties (see below).
-   `section` (optional) is used by the IDE to place the option under a submenu. By default no submenu is used (i.e., the option is just appended to the corresponding menu)
-   `needsConfirmation` (optional) specifies whether the change should be confirmed by the user. By default is `false.`.

## Example

Lets take a look at one of the changes pre-defined in the API: `RenameClass`.
As detailed [here](../changes/post.md), this change must look like the following in order to be valid for its application:

```json
{
	"type": "RenameClass",
	"className": "MyClass",
	"newName": "OurClass",
	"renameReferences": true
}
```

Here, the meta-model object to which the change refers is a class named `MyClass`, and to which the change aims at renaming it as `OurClass`, renaming also all references (i.e., methods referencing `MyClass`).

Now, lets express our pre-defined class rename change as the definition of a new extended one, using a different change type.

```json
{
	"extensionType": "change",
	"elementType": "class",
	"label": "My own class renaming",
	"properties": {
		"type": "RenameClass",
		"className": "{element.name}",
		"renameReferences": true,
		"newName": "{parameters.newName}"
	},
	"parameters": [
		{
			"name": "newName",
			"label": "New name",
			"defaultValue": "{element.name}",
			"type": "input"
		}
	]
}
```

## Properties

As said above, `properties` represent the actual properties the change will have. Their values however, will be _processed_ to obtaing the actual values in the following way.

### Element attributes

If the value of a property contains `{element.xxx}`, the actual value will be extracted from the attribute corresponding to what follows the dot (`xxx`). Here, `element` represents the meta-model object from which the change is triggered (most likely the object selected in IDE).
In the example, the value for `className` property is `{element.name}`, meaning that the actual value will be extracted from the attribute `name` of the target element (lets asume `MyClass` is selected).
Valid attributes can be extracted from the corresponding endpoints in the documentation ([pacakges](../code/packages/get.md), [classes](../code/classes/get.md), [variables](../code/classes/name/variables/get.md), [categories](../code/classes/name/categories/get.md), [methods](../code/methods/get.md)).

Note that an element attribute can be _immersed_ in a wider string. Moreover, there could be more than one attribute.\
As a general rule, the property value will be the result of replacing every "dot" expression (expression between curly brakets), leaving the rest of the string as it is. For example, the actual value of a property whose value is specified as `{element.methodClass} >> #{element.selector}`, applicable on a `method` element whose `methodClass` is `Point` and its `selector` is `x`, will be `Point >> x`.

### Parameter values

If the value contains `{parameters.xxx}`, the actual value will be extracted from the parameter with the name given by what follows the dot (`xxx`). In our example, the parameter named `newName` will be first prompted to the user, and once the user has provided a value for it, such value will be used for the property.

As in the case of element attributes, there might be more than one occurrence of such pattern, and those will be treated as explained above.

### Fixed values

Finally, if none of the above conditions hold, the value of a property is treated as a fixed value. This is the case of the properties `type` and `renameReferences` for which the values ar `RenameClass` and `true` respectively.

## Parameters

As shown in our example, a parameter has the following form:

```json
{
	"name": "string",
	"label": "string",
	"defaultValue": "value",
	"type": "input"
}
```

At the moment of writing this documentation there is only one `type`: `input`.

The `defaultValue` can contain zero or more element attribute expressions ({`element.xxx`}) spread accross a fixed string.\
In our example, the default value is the current name of the class, and so it is defined as `{element.name}`.

With this specification, users will find an option _My own class renaming_ withing the class menu options, and when they hit it, they will be prompted for _New name_, with the curren name as the default value. Should they accept the prompt, the IDE will post the following change:

```json
{
	"type": "RenameClass",
	"className": "MyClass",
	"newName": "OurClass",
	"renameReferences": true
}
```

## Another example: move a method to superclass

Lets suppose we want to extend method options to include one for moving the selected method to its superclass (available in some IDEs such as Pharo's one).
We should include the following extension:

```json
{
	"type": "MethodChange",
	"label": "Move to superclass",
	"properties": {
		"type": "MoveUpMethod",
		"className": "{element.methodClass}",
		"selector": "{element.selector}"
	},
	"parameters": []
}
```

And make sure our target system manages `MoveUpMethod` change properly.
