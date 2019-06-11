# memoman
A hackable spaced-repetition system. In development.

For a very comprehensive discussion/introduction to spaced repetition, see
[gwern: Spaced Repetition for Efficient Learning](https://www.gwern.net/Spaced-repetition).
The ideas in the section ["prospects: Extended Flashcards](https://www.gwern.net/Spaced-repetition#prospects-extended-flashcards) are a large part of the motivation for this project.

## The source:
- SuperMemo.hs contains a simple implementation of the SM2 algorithm for spaced-repetition.
- Database.hs contains the system for managing the metadata, associating file paths, ids and SM2 learning data.
- SerialUUID.hs contains an orphan instance of `Serializable` for the `UUID` type of `Data.UUID`.

## TODO:
- Profiling of the database system for very large card collections.
- Implement basic card formats/types. (somewhat done)
- Implement CLI review. (Done)
- De-spaghettify, fix encapsulation of the various modules.
- Writer proper robust parsing code.