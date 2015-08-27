Code.require_file "documentation.exs", __DIR__
Code.require_file "informant.exs", __DIR__

defmodule Alchemist.Source do
  @moduledoc false

  alias Alchemist.Documentation
  alias Alchemist.Informant

  def find([nil, function, [context: _, imports: [], aliases: _]]) do
    function
    |> look_for_kernel_functions
  end

  def find([nil, function, [context: _, imports: imports, aliases: _ ]]) do
    module = Enum.filter(imports, &Informant.has_function?(&1, function))
    |> List.first

    case module do
      nil -> look_for_kernel_functions(function)
      _ -> source(module)
    end
  end

  def find([module, _function, [context: _, imports: _, aliases: aliases]]) do
    case module |> Module.split do
      [] -> module
      module -> expand_alias(module, aliases)
    end |> source
  end

  defp look_for_kernel_functions(function) do
    cond do
      Documentation.func_doc?(Kernel, function) ->
        source(Kernel)
      Documentation.func_doc?(Kernel.SpecialForms, function) ->
        source(Kernel.SpecialForms)
      true -> ""
    end
  end

  defp source([]), do: nil
  defp source([module]) do
    module = Module.concat [module]
    do_source(module)
  end
  defp source(module), do: do_source(module)

  defp do_source(module) do
    if Code.ensure_loaded? module do
      case module.module_info(:compile)[:source] do
        nil    -> nil
        source -> List.to_string(source)
      end
    end
  end

  defp expand_alias([name | rest] = list, aliases) do
    module = Module.concat(Elixir, name)
    Enum.find_value aliases, list, fn {alias, mod} ->
      if alias === module do
        case Atom.to_string(mod) do
          "Elixir." <> mod ->
            Module.concat [mod|rest]
          _ ->
            mod
        end
      end
    end
  end

end
