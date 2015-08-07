Code.require_file "documentation.exs", __DIR__

defmodule Alchemist.Case do

  alias Alchemist.Completer
  alias Alchemist.Informant
  alias Alchemist.Documentation

  defmodule Complete do
    def process! do
      Completer.run('')
      |> Enum.map &IO.puts('cmp:' ++ &1)
      IO.puts "END-OF-COMPLETE"
    end

    def process!(hint) do
      [hint, modules, aliases] = String.split(hint, ";", parts: 3)
      process!(hint, modules, aliases)
    end

    def process!(hint, modules, aliases) do
      {modules, _} = Code.eval_string(modules)
      {aliases, _} = Code.eval_string(aliases)

      Application.put_env(:"alchemist.el", :aliases, aliases)

      funcs = for module <- modules do
        Informant.get_functions(module, hint)
      end
      |> List.flatten
      |> Enum.map &Kernel.to_string/1

      completes = Completer.run(hint) |> Enum.map &Kernel.to_string/1

      funcs ++ completes
      |> Enum.uniq
      |> Enum.map &IO.puts('cmp:' ++ &1)

      IO.puts "END-OF-COMPLETE"
    end
  end

  defmodule Modules do
    def process! do
      modules = Completer.run(':')
      |> Enum.map &Module.split/1
      functions = Completer.run('')
      |> Enum.map &Kernel.to_string/1
      modules ++ functions |> Enum.map &IO.puts/1
      IO.puts "END-OF-MODULES"
    end
  end

  defmodule Doc do
    def process!(exp) do
      [exp, modules] = String.split(exp, ";", parts: 2)
      {modules, _} = Code.eval_string(modules)
      process!(exp, modules)
    end

    def process!(exp, []) do
      Documentation.search(exp)
      IO.puts "END-OF-DOC"
    end

    def process!(exp, modules) do
      Documentation.search(exp, modules)
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
    def process!(exp) do
      [module, function] = String.split(exp, ",", parts: 2)
      {module, _} = Code.eval_string module
      function = String.to_atom function
      Alchemist.Source.find(module, function)
      IO.puts "END-OF-SOURCE"
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
