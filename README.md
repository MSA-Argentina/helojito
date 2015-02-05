Helojito
========
[Relojito](https://github.com/MSA-Argentina/relojito) CLI Tool

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

```
> helojito task week
|Monday  |Tuesday |Wednesd |Thursda |Friday  |Saturda |Sunday
|0.0hs   |0.0hs   |0.0hs   |6.0hs   |2.0hs   |0.0hs   |0.0hs
                           |10      |4
                           |8
                           |9
                           |7
                           |5
```

```
> helojito task day
Wednesday
----------
Total: 4.5hs
----------
15 - ugh: 4.0hs
11 - malboro1: 0.5hs
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

Adding a task:

```
> helojito task add -n "title baz" -t 2.5 -p 1 -y 1 -r 1 -d "description something" -w "2014-04-31"
```

Adding a task from a commit. Will extract it's date and message (as name) of the task:

```
> helojito task commit -h 4 -t1 -p1 dffae1e
```

Updating a task:

```
> helojito task mod 5 -n "new title" -h 1.5
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
Desirable because GHC may be too fast for official Ubuntu repositories:

http://haskell-lang.org/downloads/linux

### Thanks
Inspired in dmjio's [hackernews](https://github.com/dmjio/hackernews/) API.
