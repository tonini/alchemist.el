Code.require_file "server/io.exs", __DIR__
Code.require_file "server/socket.exs", __DIR__

defmodule Alchemist.Server do

  @moduledoc """
  The Alchemist-Server operates as an informant for a specific desired
  Elixir Mix project and serves with informations as the following:

    * Completion for Modules and functions.
    * Documentation lookup for Modules and functions.
    * Code evaluation and quoted representation of code.
    * Definition lookup of code.
    * Listing of all available Mix tasks.
    * Listing of all available Modules with documentation.
  """

  alias Alchemist.Server.IO, as: ServerIO
  alias Alchemist.Server.Socket, as: ServerSocket

  def start([args]) do
    {opts, _, _} = OptionParser.parse(args, switches: [port: :integer])
    env = Keyword.get(opts, :env, "dev")
    noansi = Keyword.get(opts, :no_ansi, false)
    Application.put_env(:iex, :colors, [enabled: !noansi])
    case Keyword.get(opts, :listen, false) do
      false -> ServerIO.start([env: env])
      true -> ServerSocket.start(opts)
    end
    :timer.sleep :infinity
  end
end
