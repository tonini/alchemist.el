Code.require_file "../helpers/complete.exs", __DIR__
Code.require_file "../helpers/response.exs", __DIR__

defmodule Alchemist.API.Comp do

  @moduledoc false

  alias Alchemist.Helpers.Complete
  alias Alchemist.Helpers.Response

  def request(args) do
    args
    |> normalize
    |> process
  end

  def process([nil, _, imports, _]) do
    Complete.run('', imports) ++ Complete.run('')
    |> response
  end

  def process([hint, _context, imports, aliases]) do
    Application.put_env(:"alchemist.el", :aliases, aliases)

    Complete.run(hint, imports) ++ Complete.run(hint)
    |> response
  end

  defp normalize(request) do
    {{hint, [ context: context,
              imports: imports,
              aliases: aliases ]}, _} =  Code.eval_string(request)
    [hint, context, imports, aliases]
  end

  defp response(result) do
    result
    |> Enum.join("\n")
    |> Response.endmark("COMP")
  end

end
