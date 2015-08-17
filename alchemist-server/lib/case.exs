Code.require_file "documentation.exs", __DIR__

defmodule Alchemist.Case do

  alias Alchemist.Completer
  alias Alchemist.Informant
  alias Alchemist.Documentation

  defmodule Complete do
    def process! do
      Completer.run('') |> print
    end

    def process!(input) when is_binary(input) do
      input
      |> normalize
      |> process!
    end

    def process!([hint, modules, aliases]) do
      Application.put_env(:"alchemist.el", :aliases, aliases)

      funcs = for module <- modules do
        Informant.get_functions(module, hint)
      end |> List.flatten
      candidates = Completer.run(hint)

      print(funcs ++ candidates)
    end

    def normalize(input) do
      [hint, modules, aliases] = String.split(input, ";", parts: 3)
      {modules, _} = Code.eval_string(modules)
      {aliases, _} = Code.eval_string(aliases)
      [hint, modules, aliases]
    end

    def print(result) do
      result
      |> Enum.uniq
      |> Enum.map &IO.puts/1
      IO.puts "END-OF-COMPLETE"
    end
  end

  defmodule Modules do
    def process! do
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
    def process!(input) when is_binary(input) do
      input
      |> normalize
      |> process!
    end

    def process!([expr]) do
      Documentation.search(expr)
      print
    end

    def process!([expr, modules]) do
      Documentation.search(expr, modules)
      print
    end

    def normalize(input) do
      [expr, modules] = String.split(input, ";", parts: 2)
      {modules, _}    = Code.eval_string(modules)
      if modules do
        [expr, modules]
      else
        [expr]
      end
    end

    def print do
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

  defmodule Find do
    def process!(input) do
      input
      |> normalize
      |> Alchemist.Source.find
      |> IO.puts

      IO.puts "END-OF-SOURCE"
    end

    def normalize(input) do
      [module, function] = String.split(input, ",", parts: 2)
      {module, _} = Code.eval_string module
      function    = String.to_atom function
      [module, function]
    end
  end

  defmodule MixTask do
    def process! do
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
