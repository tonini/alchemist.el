defmodule Alchemist.Helpers.ModuleInfo do

  @moduledoc false

  def moduledoc?(module) do
    case Code.get_docs module, :moduledoc do
      {_, doc} -> is_binary doc
      _ -> false
    end
  end

  def docs?(module, function) do
    docs = Code.get_docs module, :docs
    do_docs?(docs, function)
  end

  def expand_alias([name | rest] = list, aliases) do
    module = Module.concat(Elixir, name)
    Enum.find_value(aliases, list, fn {alias, mod} ->
      if alias === module do
        case Atom.to_string(mod) do
          "Elixir." <> mod ->
            Module.concat [mod|rest]
          _ ->
            mod
        end
      end
    end) |> normalize_module
  end

  def get_functions(module, hint) do
    hint        = to_string hint
    {module, _} = Code.eval_string(module)
    functions   = get_module_funs(module)

    list = Enum.reduce functions, [], fn({f, a}, acc) ->
      case :lists.keyfind(f, 1, acc) do
        {f, aa} -> :lists.keyreplace(f, 1, acc, {f, [a|aa]})
        false -> [{f, [a]}|acc]
      end
    end

    do_get_functions(list, hint) |> :lists.sort()
  end

  def has_function?(module, function) do
    List.keymember? get_module_funs(module), function, 0
  end

  defp do_get_functions(list, "") do
    all_functions(list)
  end

  defp do_get_functions(list, hint) do
    all_functions(list, hint)
  end

  defp get_module_funs(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        module.module_info(:functions) ++ module.__info__(:macros)
      _otherwise ->
        []
    end
  end

  defp all_functions(list) do
    for {fun, arities} <- list, name = Atom.to_string(fun) do
      "#{name}/#{List.first(arities)}"
    end
  end

  defp all_functions(list, hint) do
    for {fun, arities} <- list,
    name = Atom.to_string(fun),
    String.starts_with?(name, hint) do
      "#{name}/#{List.first(arities)}"
    end
  end

  def all_applications_modules do
    for [app] <- loaded_applications(),
    {:ok, modules} = :application.get_key(app, :modules),
    module <- modules do
      module
    end
  end

  defp do_docs?([head|tail], function) do
    {{func, _}, _, _, _, doc} = head
    if func == function and is_binary(doc) do
      true
    else
      do_docs?(tail, function)
    end
  end
  defp do_docs?([], _function), do: false
  defp do_docs?(nil, _function), do: false

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end

  defp normalize_module(mod) do
    if is_list(mod) do
      Module.concat(mod)
    else
      mod
    end
  end

end
