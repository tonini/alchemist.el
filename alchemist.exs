defmodule Alchemist do

  defmodule Autocomplete do
    def expand(exp) do
      code = case is_bitstring(exp) do
               true -> exp |> String.to_char_list
               _ -> exp
             end

      {status, result, list } = IEx.Autocomplete.expand(code |> Enum.reverse)

      case { status, result, list } do
        { :no, _, _ }  -> ''
        { :yes, [], _ } -> List.insert_at(list, 0, exp)
        { :yes, _,  _ } -> expand(code ++ result)
      end
    end
  end

  defmodule Source do

    def find(nil, function) do
      cond do
        List.keymember?(get_module_funs(Kernel), function, 0) ->
          IO.puts source(Kernel)
        List.keymember?(get_module_funs(Kernel.SpecialForms), function, 0) ->
          IO.puts source(Kernel.SpecialForms)
        true ->
          IO.puts ""
      end
    end

    def find(module, function) do
      cond do
        Code.ensure_loaded?(module) ->
          IO.puts source(module)
        List.keymember?(Kernel.module_info[:exports], function, 0) ->
          IO.puts source(Kernel)
        List.keymember?(Kernel.SpecialForms.module_info[:exports], function, 0) ->
          IO.puts source(Kernel.SpecialForms)
        true ->
          IO.puts ""
      end
    end

    defp source(module) do
      source = module.module_info(:compile)[:source]

      case source do
        nil -> nil
        source -> "source-file-path:" <> List.to_string(source)
      end
    end

    defp get_module_funs(mod) do
      if function_exported?(mod, :__info__, 1) do
        if docs = Code.get_docs(mod, :docs) do
          for {tuple, _line, _kind, _sign, doc} <- docs, doc != false, do: tuple
        else
        (mod.__info__(:functions) -- [__info__: 1]) ++ mod.__info__(:macros)
        end
      else
        mod.module_info(:exports)
      end
    end
  end

  defmodule Documentation do
    def search(exp) do
      Code.eval_string("import IEx.Helpers \nApplication.put_env(:iex, :colors, [enabled: true])\nh(#{exp})", [], __ENV__)
    end
  end

  defmodule Server do
    def start([env]) do
      # Preload Enum so we load basic Elixir/Erlang code
      IEx.Autocomplete.expand('.munE')
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
        ["COMPLETE", exp] ->
          Autocomplete.expand(exp) |> Enum.map fn (f) -> IO.puts('cmp:' ++ f) end
          IO.puts "END-OF-COMPLETE"
        ["DOC", exp] ->
            Documentation.search(exp)
            IO.puts "END-OF-DOC"
        ["SOURCE", exp] ->
          [module, function] = String.split(exp, ",", parts: 2)
          module = String.to_char_list module
          function = String.to_atom function
          Code.eval_string("Source.find(#{module}, :#{function})", [], __ENV__)
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
end

Alchemist.Server.start([System.argv])
