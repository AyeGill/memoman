# memoman
A hackable spaced-repetition system. In development.

For a very comprehensive discussion/introduction to spaced repetition, see
[gwern: Spaced Repetition for Efficient Learning](https://www.gwern.net/Spaced-repetition).
The ideas in the section ["prospects: Extended Flashcards](https://www.gwern.net/Spaced-repetition#prospects-extended-flashcards) are a large part of the motivation for this project.

## The source:

- SuperMemo.hs contains a simple implementation of the SM2 algorithm for spaced-repetition.
- Database.hs contains the system for managing the metadata, associating file paths, ids and SM2 learning data.
- CliViewer.hs contains code for printing out q/a pairs and getting back q-values.
- SimpleCard.hs contains code for reading cards out of a simple raw-text format:
- SerialUUID.hs contains an orphan instance of `Serializable` for the `UUID` type of `Data.UUID`.

## SimpleCard
The format parsed by SimpleCard.hs is as follows:
Each card consists of:

- A line starting with `ID:`. Leave the rest of the line blank when making cards - the script will give them a UUID. For files that have been processed by the script, the colon will be followed by a UUID.
- A block of text, giving the question for this card. The question should not contain blank lines.
- A blank line
- The answer.
- A line containing only the characters "---".
The last card may omit the "---".

## TODO:

- Profiling of the database system for very large card collections.
- Implement basic card formats/types. (somewhat done)
- Implement CLI review. (Done)
- De-spaghettify, fix encapsulation of the various modules.
- Writer proper robust parsing code.
- Think about the initial delays - seem to be very different from Anki??
- Dynamic cards with multiple ids?

