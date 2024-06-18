# Retrieve extensions

These are extensions that apply or are meant to be used specifically by the IDE. The gaining here is that each especific backend (dialect) can benefit from this endpoint to populate the IDE with the options they are used to offer in its native IDE.

| Method | Path                  | Description                           | Parameters | Payload |
| :----: | --------------------- | ------------------------------------- | :--------: | ------- |
|  GET   | [/extensions](get.md) | Retrieve list of extension defintions |     -      | -       |

**URL**: `/extensions`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

By the moment, the following types are available:

-   [**Change extensions**](changes.md)
-   [**Download extensions**](downloads.md)
-   [**Search extensions**](searches.md)
