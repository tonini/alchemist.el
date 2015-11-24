Code.require_file "../helpers/complete.exs", __DIR__

defmodule Alchemist.API.Comp do

  @moduledoc false

  alias Alchemist.Helpers.Complete

  def request(args) do
    args
    |> normalize
    |> process
  end

  def process([nil, _, imports, _]) do
    Complete.run('', imports) ++ Complete.run('')
    |> print
  end

  def process([hint, _context, imports, aliases]) do
    Application.put_env(:"alchemist.el", :aliases, aliases)

    Complete.run(hint, imports) ++ Complete.run(hint)
    |> print
  end

  defp normalize(request) do
    {{hint, [ context: context,
              imports: imports,
              aliases: aliases ]}, _} =  Code.eval_string(request)
    [hint, context, imports, aliases]
  end

  defp print(result) do
    result
    |> Enum.uniq
    |> Enum.map(&IO.puts/1)

    IO.puts "END-OF-COMP"
  end
end
