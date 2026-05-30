# tododleoo 📝

A small command-line todo manager written in Haskell.

This is a learning project (my first Haskell code), so it is deliberately simple and still rough around the
edges. The domain logic is pure and property-tested; the command-line layer is
plainer and likely to change.


## Building and running

Built with [Stack](https://docs.haskellstack.org/).

```bash
stack build      # Compile the library, executable and tests
stack run        # Start the interactive prompt
stack test       # Run the QuickCheck suite
```


## Usage

`tododleoo` runs as an interactive prompt. On first run it asks whether to
create a store, then accepts the following commands:

| Command        | Description                          |
| -------------- | ------------------------------------ |
| `add <title>`  | Add a new todo                       |
| `done <id>`    | Mark a todo as completed             |
| `remove <id>`  | Delete a todo                        |
| `view <id>`    | Show a single todo                   |
| `list`         | Show all todos                       |
| `help`         | Reprint the command list             |
| `quit` / `exit`| Leave                                |

Titles are trimmed of surrounding whitespace, must not be empty, and are
capped at 256 characters.


## Storage

Todos are saved as JSON under your XDG data directory, typically
`~/.local/share/todoleoo/todoleoos.json`. Writes are atomic: the store is
written to a temporary file and then renamed over the original, so an
interrupted save will not corrupt your existing todos.


## Status

The pure core (`Domain` and `Domain.Internal`) is the most
developed part, with a QuickCheck suite covering title parsing, ID assignment,
deletion and completion. Known gaps include friendlier command-line output,
graceful handling of end-of-input, and round-trip tests for the persistence
layer.
