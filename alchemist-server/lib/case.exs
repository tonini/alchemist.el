Code.require_file "documentation.exs", __DIR__

defmodule Alchemist.Case do
  @moduledoc false

  alias Alchemist.Completer
  alias Alchemist.Informant
  alias Alchemist.Documentation

  defmodule Complete do
    def process do
      Completer.run('') |> print
    end

    def process(request) when is_binary(request) do
      request
      |> normalize
      |> process
    end

    def process({expr,
                  [ context: _context,
                    imports: imports,
                    aliases: aliases ]}) do
      Application.put_env(:"alchemist.el", :aliases, aliases)

      funcs = for module <- imports do
        Informant.get_functions(module, expr)
      end |> List.flatten
      candidates = Completer.run(expr)

      print(funcs ++ candidates)
    end

    def normalize(request) do
      {{expr, context_info}, _} = Code.eval_string(request)
      {expr, context_info}
    end

    def print(result) do
      result
      |> Enum.uniq
      |> Enum.map &IO.puts/1
      IO.puts "END-OF-COMPLETE"
    end
  end

  defmodule Modules do
    def process do
      modules = Informant.all_applications_modules
      |> Enum.uniq
      |> Enum.reject(&is_nil/1)
      |> Enum.filter(&Documentation.moduledoc?/1)

      functions = Completer.run('')
      print(modules ++ functions)
    end

    def print(result) do
      result
      |> Enum.uniq
      |> Enum.map &IO.puts/1
      IO.puts "END-OF-MODULES"
    end
  end

  defmodule Doc do
    def process(request) when is_binary(request) do
      request
      |> normalize
      |> process
    end

    def process([expr, modules, aliases]) do
      Documentation.search(expr, modules, aliases)
      print
    end

    def normalize(request) do
      {{expr, [context: _, imports: imports, aliases: aliases]}, _} = Code.eval_string(request)
      [expr, imports, aliases]
    end

    def print do
      IO.puts "END-OF-DOC"
    end
  end

  defmodule Eval do
    def process(file) do
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
    def process(file) do
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

  defmodule Find do
    def process(request) do
      request
      |> normalize
      |> Alchemist.Source.find
      |> IO.puts

      IO.puts "END-OF-SOURCE"
    end

    def normalize(request) do
      {{expr, context_info}, _} = Code.eval_string(request)
      [module, function] = String.split(expr, ",", parts: 2)
      {module, _} = Code.eval_string(module)
      function = String.to_atom function
      [module, function, context_info]
    end
  end

  defmodule MixTask do
    def process do
      # append things like hex or phoenix archives to the load_path
      Mix.Local.append_archives

      tasks =
        Mix.Task.load_tasks(:code.get_path)
        |> Enum.map(&Mix.Task.task_name/1)

      for info <- Enum.sort(tasks) do
        IO.puts info
      end

      IO.puts "END-OF-MIXTASKS"
    end
  end
end
