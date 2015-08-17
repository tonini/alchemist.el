defmodule Alchemist.Utils do
  def clear_context_list(modules) do
    cleared = Regex.replace ~r/\.\]/, modules, "]"
    Regex.replace ~r/\.\,/, cleared, ","
  end
end
