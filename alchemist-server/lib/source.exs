Code.require_file "documentation.exs", __DIR__

defmodule Alchemist.Source do

  alias Alchemist.Documentation

  def find([nil, function, _context_info]) do
    cond do
      Documentation.func_doc?(Kernel, function) ->
        source(Kernel)
      Documentation.func_doc?(Kernel.SpecialForms, function) ->
        source(Kernel.SpecialForms)
      true -> ""
    end
  end

  def find([module, _function, _context_info]) do
    if Code.ensure_loaded?(module) do
      source(module)
    else
      ""
    end
  end

  defp source(module) do
    case module.module_info(:compile)[:source] do
      nil    -> nil
      source -> List.to_string(source)
    end
  end
end
