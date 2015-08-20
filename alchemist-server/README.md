[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://img.shields.io/travis/tonini/alchemist-server.svg)](https://travis-ci.org/tonini/alchemist-server)

**INFO:** The Alchemist-Server is in Beta status and feedback and critic are highly appreciated.

# Alchemist Server

The Alchemist-Server operates as an informant for a specific desired
Elixir Mix project and serves with informations as the following:

* Completion for Modules and functions.
* Documentation lookup for Modules and functions.
* Code evaluation and quoted representation of code.
* Definition lookup of code.
* Listing of all available Mix tasks.
* Listing of all available Modules with documentation.

# Usage

The server needs to be started inside an Elixir mix project like below:

```
$ cd elixir_project
$ elixir path/to/alchemist-server/run.exs dev
Alchemist-Server (0.7.0) - press Ctrl+C to exit
```

The Alchemist-Server API is STDIN/STDOUT based, when input sent to a
running server process it responds by sending information back to the STDOUT.

A request consisting of two parts, the request type and the request arguments.

Example for a completion request:

```
[type]   [arguments]

COMPLETE { "def" [ context: Elixir, imports: [Enum], aliases: [{MyList, List}] ] }
```

# API

## Completion

Return a completion list of all the available candidates.

```
COMPLETE
COMPLETE { "def" [ context: Elixir, imports: [], aliases: [] ] }
COMPLETE { "List.fla" [ context: Elixir, imports: [], aliases: [] ] }
```

## Documentation lookup

Return the documentation.

```
DOC { "defmodule" [ context: Elixir, imports: [], aliases: [] ] }
DOC { "List.flatten/1" [ context: Elixir, imports: [], aliases: [] ] }
```

## Evaluation

Return the evaluation result of the code from the file.

```
EVAL path/to/file/which/holds/content/to/eval.tmp
```

## Quoted

Return the code from the file quoted.

```
QUOTE path/to/file/which/holds/content/to/quote.tmp
```

## Definition lookup

Return the path to the source file which holds the definition.

```
SOURCE  { "List,flatten", [ context: [], imports: [], aliases: [] ] }
SOURCE  { "nil,defmacro", [ context: [], imports: [], aliases: [] ] }
SOURCE  { "String,nil", [ context: [], imports: [], aliases: [] ] }
```

## Mixtasks

Return a list of all available mix tasks.

```
MIXTASKS
```

## Modules

Return a list of all available modules which has documentation.

```
MODULES
```
