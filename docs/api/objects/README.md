# Objects
Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/objects](get.md) | Retrive pinned objects | - | | - |
| GET | [/objects/{id}](id/get.md) | Retrieve the pinned object with a given ID | - | | - |
| DELETE | [/objects/{id}](id/delete.md) | Unpin the object with a given ID | - | | - |
| GET | [/objects/{id}/*](id/slots/get.md) | Retrive an inner slot (a any depth) starting from the pinned object with ID | - | | - |