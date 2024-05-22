# Retrieve catesuperclassesgories

Retrieve superclasses of a given class.

**URL**: `/class/{name}/superclasses`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `[string]`

**Example:**: `Integer` subclasses `GET /classes/Integer/superclasses`.

````json
[
	{
		"name": "Number",
		"definition": "Magnitude subclass: #Number\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "Magnitude",
		"comment": "Class Number holds the most general methods for dealing with numbers. Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity.\r\rAll of Number's subclasses participate in a simple type coercion mechanism that supports mixed-mode arithmetic and comparisons.  It works as follows:  If\r\tself<typeA> op: arg<typeB>\rfails because of incompatible types, then it is retried in the following guise:\r\t(arg adaptTypeA: self) op: arg adaptToTypeA.\rThis gives the arg of typeB an opportunity to resolve the incompatibility, knowing exactly what two types are involved.  If self is more general, then arg will be converted, and viceVersa.  This mechanism is extensible to any new number classes that one might wish to add to Pharo.  The only requirement is that every subclass of Number must support a pair of conversion methods specific to each of the other subclasses of Number.\r\rImplementation notes\r----------------------------------\rThe implementation of #degreeCos and #degreeSin is such that results are exact for any multiple of 90.\r\rCare is also taken to evaluate the sine between -90° and 90°, this will avoid #degreesToRadians and i386 FPU sine function to accumulate round off errors due to approximate representation of pi.\rWe can thus evaluate 240 degreeCos with at most 1 ulp error. It's not perfect, but better than previous implementation.\r\rFor cosine, we know that:\r\tcosd(x)=cosd(abs(x))\r\tcosd(x)=sind(90-x)\rthus the trick is to evaluate:\r\tcosd(x)=sind(90-abs(x)) after appropriate modulo in [-180,180[\rThis way, we are sure to evaluate the sine between -90° and 90°\rThe #degreesToRadians and #sin are used rather than #degreeSin to avoid cycles.\r\rFor sine, it would be necessary to evaluate either\rsind(x) if abs(x) <=90\ror sind(180-x) if abs(x) >= 90\rA possible implementation would be:\r\t| x |\r\tx := 90 + self \\\\ 360 - 90.\r\tx >= 180 ifTrue: [x := 180 - x].\r\t^x degreesToRadians sin\rWe prefer evaluating cosd(90-x) thus providing a branch free implementation.",
		"category": "Numbers",
		"variable": false,
		"package": "Kernel"
	},
	{
		"name": "Magnitude",
		"definition": "Object subclass: #Magnitude\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Numbers'",
		"superclass": "Object",
		"comment": "I'm the abstract class Magnitude that provides common protocol for objects that have\rthe ability to be compared along a linear dimension, such as dates or times.\rSubclasses of Magnitude include Date, ArithmeticValue, and Time, as well as\rCharacter and LookupKey.\r \r \rMy subclasses should implement\r  < aMagnitude \r  = aMagnitude \r  hash\r\rHere are some example of my protocol:\r     3 > 4\r     5 = 6\r     100 max: 9\r\t7 between: 5 and: 10 \r",
		"category": "Numbers",
		"variable": false,
		"package": "Kernel"
	},
	{
		"name": "Object",
		"definition": "ProtoObject subclass: #Object\r\tinstanceVariableNames: ''\r\tclassVariableNames: 'DependentsFields'\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Objects'",
		"superclass": "ProtoObject",
		"comment": "`Object` is the root class for almost all of the other classes in the class hierarchy. The exceptions are ProtoObject (the superclass of Object) and its subclasses.\r\rClass `Object` provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message sending, and reflection. Also utility messages that all objects should respond to are defined here.\r\r`Object` has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that have special implementations (SmallInteger and UndefinedObject for example) or the VM knows about and depends on the structure and layout of certain standard classes.\r\rClass Variables:\r\tDependentsFields\t\tan IdentityDictionary\r\t\tProvides a virtual 'dependents' field so that any object may have one\r\t\tor more dependent views, synchronized by the changed:/update: protocol.\r\t\tNote that class Model has a real slot for its dependents, and overrides\r\t\tthe associated protocol with more efficient implementations.\r\rBecause `Object` is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed by certain subsystems or applications, or to respond to certain general test messages such as isMorph.\r\r\r###Miscellanous Discussions\r\rAbout `at:` index accepting float and not only integers\r \rThis behavior is also free in the sense that the failure code is only invoked when the primitive fails and so adds nothing to the cost of successful accesses, which are the high dynamic frequency operation.  It will also show up under\rprofiling if one is concerned about efficiency, and so isn't a hidden cost.\r\rIt is also in keeping with Pharo's mixed mode/arbitrary precision\rimplicit coercion number system that one *can* use fractions or floats as\rindices.  Stripping out coercions like this will make the system more brittle.  So \rplease do *not* remove this hack.  I think it's a feature and a useful one.\r\rCan you give me an example that demonstrates the usefulness of this feature?\r\r```\r| a r |\ra := Array new: 10 withAll: 0.\rr := Random new.\r100 timesRepeat: [| v | v := r next * 10 + 1. a at: v put: (a at: v) + 1].\ra\r```\r\ri.e. I didn't have to provide an explicit rounding step.  That's useful.  But in general anywhere \rwhere an index is derived by some calculation not having to provide the rounding step could be \ruseful/helpful/more concise.  e.g. (n roundTo: 0.1) * 10 vs ((n roundTo: 0.1) * 10) asInteger.\r\rSome thought went into the original choice.  It is not a hack but there by intent.  The integers are \rsimply a subset of the reals and forcing the programmer to use them is favouring the machine \rabove the programmer.\r\rBut I think you should justify getting rid of it rather than my having to justify keeping it.  Getting \rrid of it risks breaking code.  If it is there but does not harm then why get rid of it?\r\rbest Eliot Miranda ",
		"category": "Objects",
		"variable": false,
		"package": "Kernel"
	},
	{
		"name": "ProtoObject",
		"definition": "ProtoObject subclass: #ProtoObject\r\tinstanceVariableNames: ''\r\tclassVariableNames: ''\r\tpoolDictionaries: ''\r\tcategory: 'Kernel-Objects'.\rProtoObject superclass: nil",
		"superclass": null,
		"comment": "ProtoObject establishes minimal behavior required of any object in Pharo, even objects that should balk at normal object behavior. \r\rGenerally these are proxy objects designed to read themselves in from the disk, or to perform some wrapper behavior, before responding to a message. \r\rProtoObject has no instance variables, nor should any be added.",
		"category": "Objects",
		"variable": false,
		"package": "Kernel"
	}
]
````
