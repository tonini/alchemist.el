[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://img.shields.io/travis/tonini/alchemist-server.svg)](https://travis-ci.org/tonini/alchemist-server)

**INFO:** The Alchemist-Server is in Beta status and the API will most likey change until the first release. Feedback and critic are highly appreciated though.

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

The server needs to be started inside an Elixir mix project, it can be started in two ways:

## Read/Write through `STDIN/STDOUT`
```
$ cd elixir_project
$ elixir path/to/alchemist-server/run.exs --env=dev
```
In this mode, when input sent to a running server process it
responds by sending information back to the `STDOUT`.

A request consisting of two parts, the request type and the request arguments.

Example for a completion request:

```
[type]   [arguments]

COMP { "def", [ context: Elixir, imports: [Enum], aliases: [{MyList, List}] ] }
```

## Read/Write through network socket
```
$ cd elixir_project
$ elixir path/to/alchemist-server/run.exs --env=dev --listen
ok|localhost:55580
```
In this mode, when a client connects to the port, it
responds by sending information back to the opened connection

Example for a completion request:

```
$ nc localhost 55580
COMP { "def", [ context: Elixir, imports: [Enum], aliases: [{MyList, List}] ] }
```

# API

## Completion

Return a completion list of all the available candidates.

```
COMP
COMP { "def", [ context: Elixir, imports: [], aliases: [] ] }
COMP { "List.fla", [ context: Elixir, imports: [], aliases: [] ] }
```

## Documentation lookup

Return the documentation.

```
DOCL { "defmodule", [ context: Elixir, imports: [], aliases: [] ] }
DOCL { "List.flatten/1", [ context: Elixir, imports: [], aliases: [] ] }
```

## Evaluation, Quoted & Macro expand

### Evaluation

Return the evaluation result of the code from the file.

```
EVAL { :eval, 'path/to/file/which/holds/content/to/eval.tmp' }
```

### Quoted

Return the code from the file quoted.

```
EVAL { :quote, 'path/to/file/which/holds/content/to/quote.tmp' }
```

### Macro expand

Return the code from the file expanded.

```
EVAL { :expand, 'path/to/file/which/holds/content/to/expand.tmp' }
```

Return the code from the file expanded once.

```
EVAL { :expand_once, 'path/to/file/which/holds/content/to/expand_once.tmp' }
```

## Definition lookup

Return the path to the source file which holds the definition.

```
DEFL  { "List,flatten", [ context: Elixir, imports: [], aliases: [] ] }
DEFL  { "nil,defmacro", [ context: Elixir, imports: [], aliases: [] ] }
DEFL  { "nil,create_file", [ context: Elixir, imports: [Mix.Generator], aliases: [] ] }
DEFL  { "MyList,nil", [ context: Elixir, imports: [], aliases: [{MyList, List}] ] }
```

## Informations

### Mix tasks

Return a list of all available mix tasks.

```
INFO { :type, :mixtasks }
```

### Modules

Return a list of all available modules which has documentation.

```
INFO { :type, :modules }
```

### Datatype Information

Return information about any datatype.

```
INFO { :type, :info, List }
```

### Module Or Function/Arity Types Information

Return types for a module or function/arity pair.

```
INFO { :type, :types, 'List' }
INFO { :type, :types, 'Enum.t' }
INFO { :type, :types, 'Agent.on_start/0' }
```

## Debugging

Return PONG as response, it can be used for purpose of debugging and checking server's availability.

```
PING
```

## End Markers

Each request type ends with a specific end marker tag to notify that the request is done.

An end tag looks like the following:

```
END-OF-<REQUEST TYPE>
```

For example, after the following request an end tag would look like this:

```
INFO { :type, :modules }
List
String
Enum
.
...
....
END-OF-INFO
```
