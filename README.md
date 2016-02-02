hdose
=====
A simpler version of [dose](https://github.com/danilobellini/dose), a
programming Dojo helping tool, written in haskell.

## Usage
```
Usage: hdose [options] test-command
  -t n     --timeout=n    Sets the timeout in minutes for the alarm (default = 10)
  -I file  --ignore=file  A file glob to ignore
  -h       --help         Print this help message.
```

Currently, globs listed on the `.gitignore` file are also added to the ignored
list.

## Installing
This module isn't published to hackage, so you need to clone it and install it.
It's necessary that you have `cabal` installed. If you don't, you should
follow probably install the [Haskell Platform](https://www.haskell.org/platform/)
before anything else.

There's a `Makefile` which makes the rest of the process easier. You can then
run:
```bash
git clone https://github.com/yamadapc/hdose
cd hdose
make
```

And copy the produced binary at `./dist/build/hdose/hdose` to your `$PATH`.

### OSX
If you're on OSX, you'll have to install readline with `homebrew`. The
`Makefile` provided will take care of pointing its location to `cabal`.
```bash
brew install readline
```

## License
This code is licensed under the GPLv3 license. See the [LICENSE](/LICENSE) file
for more information.

## Donations
Would you like to buy me a beer? Send bitcoin to 3JjxJydvoJjTrhLL86LGMc8cNB16pTAF3y
