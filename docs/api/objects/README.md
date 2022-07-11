# Objects

Endpoints to retrieve the objects in the system, either globally accessible or as the result of evaluating an expressions (pinned objects).

| Method | Path                                | Description                                   | Parameters | Payload      |
| :----: | ----------------------------------- | --------------------------------------------- | :--------: | ------------ |
|  GET   | [/objects](get.md)                  | Retrive pinned objects                        |     -      | -            |
|  GET   | [/objects/{id}](id/get.md)          | Retrieve the pinned object with a given ID    |     -      | -            |
| DELETE | [/objects/{id}](id/delete.md)       | Unpin the object with a given ID              |     -      | -            |
|  GET   | [/objects/{id}/\*](id/slots/get.md) | Retrive the object reached thru the given URI |     -      | -            |
|  POST  | [/objects](post.md)                 | Pin the object reached thru the given URI     |     -      | `object URI` |
| DELETE | [/objects](delete.md)               | Unpin all pinned objects                      |     -      | -            |
