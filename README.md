# memoman

A hackable spaced-repetition system. In development.

For a very comprehensive discussion/introduction to spaced repetition, see
[gwern: Spaced Repetition for Efficient Learning](https://www.gwern.net/Spaced-repetition).
The ideas in the section ["prospects: Extended Flashcards](https://www.gwern.net/Spaced-repetition#prospects-extended-flashcards) are a large part of the motivation for this project.

## The source

- SuperMemo.hs contains a simple implementation of the SM2 algorithm for spaced-repetition.
  - Note that a few aspects are slightly hacked.
- Database.hs contains the system for managing the metadata, associating file paths, ids and SM2 learning data.
- CliViewer.hs contains code for printing out q/a pairs and getting back q-values.
- SimpleCard.hs contains code for reading cards out of a simple raw-text format.
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

Dynamic cards using shell scripts are also supported:
A card starting with a shebang (e.g `#/bin/bash` or `#/usr/bin/python3`) will be interpreted by running the card as a script (piping the rest of the card as standard input to the specified command) and returning whatever this process prints to standard output, where question and answer should be separated by a blank line as usual.

Currently, an error produced during such a script will be handled by printing an error message instead of the question.
This allows the user to exit the program and try to fix the error.
This behaviour could be improved in two obvious ways:

- Print the filename/id too, to make debugging less ridiculous
- Allow the user to "skip" a card when an error occurs, instead of having to enter junk data or exit the program.

## Usage

See `Main.hs` for all commands. Briefly:

- `memoman init` to create empty database in current directory.
- `memoman add-cards path` to add untracked cards in the file path to the database.
  - This will **not** complain on ill-formatted files, but simply add IDs to each line starting `ID:`, and add those IDs to the database as new cards.
- `memoman review` to review cards in need of review.
- `memoman dump-db` to print out the whole database.

## TODO

- [ ] Profiling of the database system for very large card collections.
- [x] Implement basic card formats/types. (somewhat done)
- [x] Implement CLI review. (Done)
- [ ] Web/browser-based GUI review.
- [ ] Adding cards from file which already has IDs.
- [ ] De-spaghettify, fix encapsulation of the various modules.
  - [ ] Move all logic to do with handling review into `SuperMemo.hs`.
  - [ ] Only `Database.hs` should care about the database representation.
    - [ ] Figure out division of responsibilities between SuperMemo and Database.
  - [ ] Only `SimpleCard.hs` should care about card fetching logic
  - [ ] Move logic out of `Main.hs`
  - [x] `CliViewer.hs` should care about cli interaction.
  - Currently, logic pertaining to the SuperMemo system in tangled up with the cli interaction, which is unacceptable.
- [ ] Write proper robust parsing code.
  - [ ] Including for arguments.
- [x] Use `Data.Text` and `Shelly` types everywhere.
- [ ] Write proper tests.
  - [x] Including a QuickCheck suite?
  - [ ] Refactor code to make it testable.
- [ ] Think seriously about behaviour when reviewing out of turn.
  - [ ] What happens when a card goes unreviewed for a long time?
  - [ ] Make it possible to stop mid-review and still save alterations.
  - [ ] Add option to review ahead.
- [ ] Are we implementing the repetition-within-sessions stuff correct? Especially pertaining to behaviour of delay.
