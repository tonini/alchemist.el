Code.require_file "../helpers/response.exs", __DIR__

defmodule Alchemist.API.Ping do

  @moduledoc false

  alias Alchemist.Helpers.Response

  def request do
    Response.endmark("PONG", "PING")
  end
end
