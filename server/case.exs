defmodule Alchemist.Case do

  alias Alchemist.Completer
  alias Alchemist.Utils
  alias Alchemist.Informant
  alias Alchemist.Source

  defmodule Complete do
    def process! do
      :ets.insert(:alchemist, {"aliases", []})
      Completer.run('')
      |> Enum.map &IO.puts('cmp:' ++ &1)
      print_end_of_complete_signal
    end

    def process!(hint) do
      :ets.insert(:alchemist, {"aliases", []})
      Completer.run(hint)
      |> Enum.map &IO.puts('cmp:' ++ &1)
      print_end_of_complete_signal
    end

    defp print_end_of_complete_signal do
      IO.puts "END-OF-COMPLETE"
    end

    def process_with_context!(hint) do
      [hint, modules, aliases] = String.split(hint, ";", parts: 3)
      modules = Utils.clear_context_list(modules)
      {modules, _} = Code.eval_string(modules)
      {aliases, _} = Code.eval_string(aliases)
      :ets.insert(:alchemist, {"aliases", aliases})

      Completer.run(hint)
      |> Enum.map &IO.puts('cmp:' ++ &1)
      Enum.each modules, fn(module) ->
        Informant.get_functions(module, hint)
        |> Enum.map &IO.puts('cmp:' ++ &1)
      end
      IO.puts "END-OF-COMPLETE-WITH-CONTEXT"
    end
  end

  defmodule Modules do
    def process! do
      Informant.get_modules |> Enum.map &IO.puts/1
      IO.puts "END-OF-MODULES"
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
      Code.eval_string("Source.find(#{module}, :#{function})", [], __ENV__)
      IO.puts "END-OF-SOURCE"
    end
  end
end
