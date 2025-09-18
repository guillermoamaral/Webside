# Events

This endpoint allows clients to subscribe to server-sent events..

**URL**: `/events`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

## Event Types

These are the available event types at the moment:
- **transcript**. This event is sent to append a string to the Transcript contents. Example:
  
  ```json
  {
    "event": "transcript",
    "data": "Hi from the backend!"
  }
  ``` 
- **debugger**. This event is triggered when an exception is raised at the server side and a debugger on the suspended process is created. This can be used by the IDE for example, to pop up a debugger tool pointing to that debugger. Example:
  
  ```json
  {
    "event": "debugger",
    "data": "id"
  }
  ``` 
  Where `id` is the id of the created debugger.
  
  **Note** that is not the case of an evaluation initiated through [/evaluations](../evaluations/post.md) endpoint as those should be manually handled by creating a debugger using [/debuggers](../debuggers/post.md) endpoint. The event is triggered from those that happend in the server.
- **object**. This event is sent when an action in the server has the intention of inspecting an object. Example:
  
  ```json
  {
    "event": "object",
    "data": "id"
  }
  ``` 
  Where `id` is the id of the pinned object.
  
