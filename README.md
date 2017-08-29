# servant-workshop

This repository is used as a base for a "full" [servant](http://haskell-servant.readthedocs.io/en/stable/) application, with [JWT auth](https://github.com/plow-technologies/servant-auth#readme), and testing setup and ready to go.

## Instructions

1. Install the [stack](https://docs.haskellstack.org/en/stable/README/) tool and ensure that the `~/.local/bin` directory is in your path.
2. You should have a haskell editor setup, e.g. [atom](https://github.com/simonmichael/haskell-atom-setup) or something more adventurous (e.g. [the one and true editor, spacemacs](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell))
3. Clone this repository to a handy directory.
4. Run `stack build` and `stack test` - and then run `stack install yesod-bin`

### Yesod in a servant talk??

Yes. `yesod devel` acts as a nice proxy over a web app in development, auto rebuilding the app and adjusting the proxy appropriately when it's all done. This isn't neccesary, but it's nice having a file watcher and proxy to handle web-app start ups.

## Goals

The goal of this workshop is to get all the accidental complexity out of the way, so one is left with an "already running" web api from which to customize.
