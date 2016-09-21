Code.require_file "../helpers/module_info.exs", __DIR__
Code.require_file "../helpers/response.exs", __DIR__

defmodule Alchemist.API.Defl do

  @moduledoc false

  alias Alchemist.Helpers.ModuleInfo
  alias Alchemist.Helpers.Response

  def request(args) do
    args
    |> normalize
    |> process
    |> endmark("DEFL")
  end

  defp endmark(nil , cmd) do
    "\n" <> Response.endmark(nil, cmd)
  end

  defp endmark(response , cmd) do
    Response.endmark(response, cmd)
  end


  def process([nil, function, [context: _, imports: [], aliases: _]]) do
    look_for_kernel_functions(function)
  end

  def process([nil, function, [context: _, imports: imports, aliases: _ ]]) do
    module = Enum.filter(imports, &ModuleInfo.has_function?(&1, function))
    |> List.first

    case module do
      nil -> look_for_kernel_functions(function)
      _   -> source(module)
    end
  end

  def process([module, _function, [context: _, imports: _, aliases: aliases]]) do
    if elixir_module?(module) do
      module
      |> Module.split
      |> ModuleInfo.expand_alias(aliases)
    else
      module
    end |> source
  end

  defp elixir_module?(module) do
    module == Module.concat(Elixir, module)
  end

  defp look_for_kernel_functions(function) do
    cond do
      ModuleInfo.docs?(Kernel, function) ->
        source(Kernel)
      ModuleInfo.docs?(Kernel.SpecialForms, function) ->
        source(Kernel.SpecialForms)
      true -> ""
    end
  end

  defp source([]), do: nil
  defp source(module) when is_list(module) do
    module
    |> Module.concat
    |> do_source
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

  defp normalize(request) do
    {{expr, context_info}, _} = Code.eval_string(request)
    [module, function]        = String.split(expr, ",", parts: 2)
    {module, _}               = Code.eval_string(module)
    function                  = String.to_atom function
    [module, function, context_info]
  end
end
