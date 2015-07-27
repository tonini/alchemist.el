defmodule Alchemist.Doc do

  @moduledoc false

  @doc """
  Return `true` when the `module` provides documentation.
  Otherwise, returns `false`.
  """
  def moduledoc?(module) do
    {_, doc} = Code.get_docs module, :moduledoc
    is_binary doc
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
end
