# Define a class

Define a new class or change the definition of an existing one.

**URL**: `/classes`

**Method**: `POST`

**Payload**:

```json
{
	"className": "string",
	"definition": "string",
	"instanceVariables": ["string"],
	"classVariables": ["string"],
	"poolDictionaries": ["string"]
}
```

**Note**: Property `definition` is highly dependent on the backend and can be substituted by properties `instanceVariables`, `classVariables` and `poolDictionaries`.

## Success Responses

**Code** : `200 OK`
