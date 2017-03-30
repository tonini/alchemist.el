Code.require_file "../helpers/response.exs", __DIR__

defmodule Alchemist.API.Version do

  @moduledoc false
  @version "0.1.0-beta"

  alias Alchemist.Helpers.Response

  def request do
    @version |> Response.endmark("VERSION")
  end
end
