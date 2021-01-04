# Changes
These endpoints are used to apply changes and retrieve changes made to the system.

Important: the list of possible changes at the moment of writing this documentation is by no means a closed list and it can be extended to support more changes and refactorings.

| Method | Path | Description | Parameters | Payload |
| :--: | -- | -- | :--: | -- |
| GET | [/changes](get.md) | Retrieve changes made to the system | author | - |
| POST | [/changes](post.md) | Apply a change to the system | - | `change` |