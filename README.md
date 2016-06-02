# haskell-player

A minimal graphical interface on top of `afplay` and `afinfo` built using
[brick](https://hackage.haskell.org/package/brick).

![haskell-player](haskell-player.png?raw=true)

By default it will show a list of all the contents found in the `$HOME/Music`
directory.

Commands:

* *enter* to play the selected song
* *spacebar* to pause/resume the current song
* *left arrow* to play the previous song
* *right arrow* to play the next song
* *q* to exit

## Build and run

Use stack to build and run the project:

```sh
stack build
stack exec haskell-player
```

More info on stack at http://docs.haskellstack.org/en/stable/README/.

You can download the latest executable version at [TODO: add download link].

## Requirements

It requires `afplay` and `afinfo`, that should be present in any *modern* Darwin
distribution.

## Supported formats

Any format supported by `afplay`.
