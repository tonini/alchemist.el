defmodule Alchemist do

  defmodule Utils do
    def clear_context_list(modules) do
      cleared = Regex.replace ~r/\.\]/, modules, "]"
      Regex.replace ~r/\.\,/, cleared, ","
    end
  end

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

  defmodule Function do
    def get_functions(mod, hint) do
      {mod, _} = Code.eval_string(mod)
      falist = get_module_funs(mod)

      list = Enum.reduce falist, [], fn({f, a}, acc) ->
        case :lists.keyfind(f, 1, acc) do
          {f, aa} -> :lists.keyreplace(f, 1, acc, {f, [a|aa]})
          false -> [{f, [a]}|acc]
        end
      end

      case hint do
        "" ->
          for {fun, arities} <- list,
          name = Atom.to_string(fun) do
            "#{name}/#{List.first(arities)}"
          end |> :lists.sort()
        _otherwise ->
          for {fun, arities} <- list,
          name = Atom.to_string(fun),
          String.starts_with?(name, hint) do
            "#{name}/#{List.first(arities)}"
          end |> :lists.sort()
      end
    end

    defp get_module_funs(mod) do
      case Code.ensure_loaded(mod) do
        {:module, _} ->
          mod.module_info(:functions) ++ mod.__info__(:macros)
        _otherwise ->
          []
      end

    end
  end

  defmodule Definition do
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


  defmodule Case do
    defmodule Complete do
      def process! do
        Autocomplete.expand('')
        |> Enum.map &IO.puts('cmp:' ++ &1)
        print_end_of_complete_signal
      end

      def process!(hint) do
        Autocomplete.expand(hint)
        |> Enum.map &IO.puts('cmp:' ++ &1)
        print_end_of_complete_signal
      end

      defp print_end_of_complete_signal do
        IO.puts "END-OF-COMPLETE"
      end

      def process_with_context!(hint) do
        [hint, modules] = String.split(hint, ",", parts: 2)
        modules = Utils.clear_context_list(modules)
        {modules, _} = Code.eval_string(modules)

        Autocomplete.expand(hint)
        |> Enum.map &IO.puts('cmp:' ++ &1)
        Enum.each modules, fn(module) ->
          Function.get_functions(module, hint)
          |> Enum.map &IO.puts('cmp:' ++ &1)
        end
        IO.puts "END-OF-COMPLETE-WITH-CONTEXT"
      end
    end

    defmodule Modules do
      def process! do
        get_modules |> Enum.map &IO.puts/1
        IO.puts "END-OF-MODULES"
      end

      def get_modules do
        modules = Enum.map(:code.all_loaded, fn({m, _}) -> Atom.to_string(m) end)

        if :code.get_mode() === :interactive do
          modules ++ get_modules_from_applications()
        else
          modules
        end
      end

      defp get_modules_from_applications do
        for {app, _, _} <- :application.loaded_applications,
        {_, modules} = :application.get_key(app, :modules),
        module <- modules,
        has_doc = Code.get_docs(module, :moduledoc), elem(has_doc, 1) do
          Atom.to_string(module)
        end
      end
    end

    defmodule Doc do
      def process!(exp) do
        Code.eval_string("import IEx.Helpers \nApplication.put_env(:iex, :colors, [enabled: true])\nh(#{exp})", [], __ENV__)
        IO.puts "END-OF-DOC"
      end
    end

    defmodule Eval do
      def process!(file) do
        try do
          File.read!("#{file}")
          |> Code.eval_string
          |> Tuple.to_list
          |> List.first
          |> IO.inspect
        rescue
          e -> IO.inspect e
        end
        IO.puts "END-OF-EVAL"
      end
    end

    defmodule Quote do
      def process!(file) do
        try do
          File.read!("#{file}")
          |> Code.string_to_quoted
          |> Tuple.to_list
          |> List.last
          |> IO.inspect
        rescue
          e -> IO.inspect e
        end
        IO.puts "END-OF-QUOTE"
      end
    end

    defmodule Source do
      def process!(exp) do
        [module, function] = String.split(exp, ",", parts: 2)
        module = String.to_char_list module
        function = String.to_atom function
        Code.eval_string("Definition.find(#{module}, :#{function})", [], __ENV__)
        IO.puts "END-OF-SOURCE"
      end
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
          Case.Source.process!(exp)
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
