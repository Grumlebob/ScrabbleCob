# ScrabbleCob

### Multiplayer and dictionary

Yes. The bot can play versus copies of itself, or the included bot OxyphenButazone. For playing the bot uses a GADDAG dictionary.

### Playing on all boards

No. The bot is only setup to play on the infinite board and has not been tested on any other boards.

### Parallelism (yes/no)

No. We have not used parallelism.

### Respect the timeout flag (yes/no)

No. We do not respect the timeout flag.

### How to run

The project is in the `ScrabbleTemplate` folder. It references the course libraries in `dlls/`, so keep that folder next to it.

cd into the ScrabbleTemplate folder

    cd ScrabbleTemplate

run the program

    dotnet run

## History

The branch `archive/prototype-2023-04` (tag `prototype-2023-04-10`) holds the earlier exploration, which used a trie and a GADDAG module, that this hand-in grew out of.
