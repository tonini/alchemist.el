defmodule Alchemist.Source do
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

  def find(module, nil) do
    cond do
      Code.ensure_loaded?(module) ->
        IO.puts source(module)
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
