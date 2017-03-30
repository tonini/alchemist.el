Code.require_file "../api/comp.exs", __DIR__
Code.require_file "../api/docl.exs", __DIR__
Code.require_file "../api/defl.exs", __DIR__
Code.require_file "../api/eval.exs", __DIR__
Code.require_file "../api/info.exs", __DIR__
Code.require_file "../api/ping.exs", __DIR__
Code.require_file "../api/version.exs", __DIR__

defmodule Alchemist.Helpers.ProcessCommands do

  @moduledoc false

  alias Alchemist.API

  def process(line, env) do
    loaded = all_loaded()

    paths = load_paths(env)
    apps  = load_apps(env)

    result = execute_input(line)
    purge_modules(loaded)
    purge_paths(paths)
    purge_apps(apps)
    result
  end

  defp execute_input(line) do
    case line |> String.split(" ", parts: 2) do
      ["COMP", args] ->
        API.Comp.request(args)
      ["DOCL", args] ->
        API.Docl.request(args)
      ["INFO", args] ->
        API.Info.request(args)
      ["EVAL", args] ->
        API.Eval.request(args)
      ["DEFL", args] ->
        API.Defl.request(args)
      ["PING"] ->
        API.Ping.request()
      ["VERSION"] ->
        API.Version.request()
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
