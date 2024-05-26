# Colors

This endpoint provides colors that represent the target Smalltalk dialect.\
These colors can (and will) be used on fron-end.

**URL**: `/colors`

**Method**: `GET`

## Success Responses

**Code** : `200 OK`

**Content**: `string`

**Example:**: Pharo colors `GET /colors`.

```json
{ "primary": "#3297d4", "secondary": "#1565c0" }
```
