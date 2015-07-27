defmodule Alchemist.Doc do

  @moduledoc false

  @doc """
  Return true the module provides documentation.
  Otherwise, returns false.
  """
  def moduledoc?(module) do
    {_, doc} = Code.get_docs module, :moduledoc
    is_binary doc
  end
end
