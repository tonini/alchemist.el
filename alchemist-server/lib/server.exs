Code.require_file "completer.exs", __DIR__
Code.require_file "informant.exs", __DIR__
Code.require_file "source.exs", __DIR__
Code.require_file "case.exs", __DIR__

defmodule Alchemist.Server do

  @version "0.7.0"

  @moduledoc """
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

  """

  alias Alchemist.Case

  def start([env]) do
    loop(all_loaded(), env)
  end

  def loop(loaded, env) do
    line  = IO.gets("") |> String.rstrip()
    paths = load_paths(env)
    apps  = load_apps(env)

    read_input(line)

    purge_modules(loaded)
    purge_paths(paths)
    purge_apps(apps)

    loop(loaded, env)
  end

  def read_input(line) do
    case line |> String.split(" ", parts: 2) do
      ["COMPLETE"] ->
        Case.Complete.process
      ["COMPLETE", request] ->
        Case.Complete.process(request)
      ["DOC", request] ->
        Case.Doc.process(request)
      ["MODULES"] ->
        Case.Modules.process
      ["EVAL", file] ->
        Case.Eval.process(file)
      ["QUOTE", file] ->
        Case.Quote.process(file)
      ["SOURCE", request] ->
        Case.Find.process(request)
      ["MIXTASKS"] ->
        Case.MixTask.process
      _ ->
        nil
    end
  end

  defp all_loaded() do
    for {m,_} <- :code.all_loaded, do: m
  end

  defp load_paths(env) do
    for path <- Path.wildcard("_build/#{env}/lib/*/ebin") do
      Code.prepend_path(path)
      path
    end
  end

  defp load_apps(env) do
    for path <- Path.wildcard("_build/#{env}/lib/*/ebin/*.app") do
      app = path |> Path.basename() |> Path.rootname() |> String.to_atom
      Application.load(app)
      app
    end
  end

  defp purge_modules(loaded) do
    for m <- (all_loaded() -- loaded) do
      :code.delete(m)
      :code.purge(m)
    end
  end

  defp purge_paths(paths) do
    for p <- paths, do: Code.delete_path(p)
  end

  defp purge_apps(apps) do
    for a <- apps, do: Application.unload(a)
  end
end
