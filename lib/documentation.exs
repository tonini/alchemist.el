defmodule Alchemist.Documentation do

  import IEx.Helpers, warn: false

  Application.put_env(:iex, :colors, [enabled: true])

  def search(nil) do
  end

  def search(expr) do
    try do
      Code.eval_string("h(#{expr})", [], __ENV__)
    rescue
      _e ->
    end
  end

  def search(expr, modules) do
    unless function?(expr) do
      search(expr)
    else
      modules ++ [Kernel, Kernel.SpecialForms]
      |> build_search(expr)
      |> search
    end
  end

  def build_search(modules, search) do
    function = Regex.replace(~r/\/[0-9]$/, search, "")
    function = String.to_atom(function)
    for module <- modules,
    func_doc?(module, function) do
      "#{module}.#{search}"
    end |> List.first
  end

  @doc """
  Return `true` when the `module` provides documentation.
  Otherwise, returns `false`.
  """
  def moduledoc?(module) do
    case Code.get_docs module, :moduledoc do
      {_, doc} -> is_binary doc
      _ -> false
    end
  end

  @doc """
  Return `true` when the `module` provides documentation for
  the `function`.
  """
  def func_doc?(module, function) do
    docs = Code.get_docs module, :docs
    do_func_doc?(docs, function)
  end

  defp do_func_doc?([head|tail], function) do
    {{func, _}, _, _, _, doc} = head
    if func == function and is_binary(doc) do
      true
    else
      do_func_doc?(tail, function)
    end
  end

  defp do_func_doc?([], _function) do
    false
  end

  defp do_func_doc?(nil, _function) do
    false
  end

  defp function?(expr) do
    Regex.match?(~r/^[a-z_]/, expr)
  end
end
