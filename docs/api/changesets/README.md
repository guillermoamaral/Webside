# Changesets

These endpoints are used to convert from changes in JSON format to chunk format files, particular to a given dialect, and viceversa.

| Method | Path                                | Description                                                 | Parameters | Payload  |
| :----: | ----------------------------------- | ----------------------------------------------------------- | :--------: | -------- |
|  POST  | [/changesets/download](download.md) | Convert a chunk-formatted file into a set of changes (JSON) |     -      | `change` |
|  POST  | [/changesets/upload](upload.md)     | Convert a set of changes (JSON) into a chunk formatted-file |     -      | `change` |
