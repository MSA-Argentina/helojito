Helojito
========
Relojito CLI Tool

# Installation
git clone this repo and then simply:

```
cabal install
```

The binary will end up in your `.cabal/bin/` directory.

Or you can always use a sandbox with `cabal sandbox init` before installing.

# Configuration
You'll need a file `~/.helojito` like this:

```
{
    "token": "token",
    "apiurl": "http://relojito.com/api/"
}
```

Get your token by visiting `/get_token/` in relojito.

# Basic Usage
Listing tasks:

```
> helojito task list
20 - task foo: 8.0hs - 2014-10-14
81 - another bar: 4.5hs - 2014-10-15
```

Adding tasks:

```
> helojito task add -n "title baz" -t 2.5 -p 1 -y 1 -r 1 -d "description something" -w "2014-04-31"
```

Task details:

```
> helojito task show 5
ID: 20
Name: task foo
Project: Some Project
Description:
Date: 2014-10-14
Hours Worked On: 8.0
```

List projects, resolutions and types:

```
> helojito type list
> helojito project list
> helojito resolution list
```

## Bash Completion
`helojito --bash-completion-script helojito >> ~/.bash_completion`

## Installing GHC on Ubuntu
GHC is too fast for Ubuntu repositories:

http://haskell-lang.org/downloads/linux
