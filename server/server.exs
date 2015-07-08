Code.require_file "completer.exs", __DIR__
Code.require_file "informant.exs", __DIR__
Code.require_file "source.exs", __DIR__
Code.require_file "case.exs", __DIR__

defmodule Alchemist.Server do

  alias Alchemist.Case

  def start([env]) do
    loop(all_loaded(), env)
  end

  def loop(loaded, env) do
    line  = IO.gets("") |> String.rstrip()
    paths = load_paths(env)
    apps  = load_apps(env)

    store_loaded_modules(loaded)

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
      ["COMPLETE", hint] ->
        Case.Complete.process!(hint)
      ["COMPLETE-WITH-CONTEXT", hint] ->
        Case.Complete.process_with_context!(hint)
      ["DOC", exp] ->
        Case.Doc.process!(exp)
      ["MODULES"] ->
        Case.Modules.process!
      ["EVAL", exp] ->
        Case.Eval.process!(exp)
      ["QUOTE", file] ->
        Case.Quote.process!(file)
      ["SOURCE", exp] ->
        Case.Find.process!(exp)
      ["MIXTASKS"] ->
        Case.MixTask.process!
      _ ->
        nil
    end
  end

  defp all_loaded() do
    for {m,_} <- :code.all_loaded, do: m
  end

  defp store_loaded_modules(modules) do
    Application.put_env(:"alchemist.el", :loaded_modules, modules)
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
