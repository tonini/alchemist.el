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

  def get_modules do
    Application.get_env(:"alchemist.el", :loaded_modules)
    ++ all_applications_modules
    |> Enum.uniq
    |> Stream.filter(fn(module) -> moduledoc?(module) end)
  end

  defp get_module_funs(mod) do
    case Code.ensure_loaded(mod) do
      {:module, _} ->
        mod.module_info(:functions) ++ mod.__info__(:macros)
      _otherwise ->
        []
    end
  end

  defp all_applications_modules do
    for [app] <- loaded_applications(),
    {:ok, modules} = :application.get_key(app, :modules),
    module <- modules do
      module
    end
  end

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end

  defp moduledoc?(module) when is_atom(module) do
    case Code.get_docs(module, :moduledoc) do
      {_line, moduledoc} ->
        is_binary moduledoc
      _ -> false
    end
  end
end
