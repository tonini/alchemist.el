Code.require_file "completer.exs", __DIR__
Code.require_file "informant.exs", __DIR__
Code.require_file "source.exs", __DIR__
Code.require_file "case.exs", __DIR__

defmodule Alchemist.Server do

  @version "0.7.0"

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
        Case.Complete.process!
      ["COMPLETE", request] ->
        Case.Complete.process!(request)
      ["DOC", request] ->
        Case.Doc.process!(request)
      ["MODULES"] ->
        Case.Modules.process!
      ["EVAL", file] ->
        Case.Eval.process!(file)
      ["QUOTE", file] ->
        Case.Quote.process!(file)
      ["SOURCE", request] ->
        Case.Find.process!(request)
      ["MIXTASKS"] ->
        Case.MixTask.process!
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
