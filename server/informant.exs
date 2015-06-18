defmodule Alchemist.Informant do
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
