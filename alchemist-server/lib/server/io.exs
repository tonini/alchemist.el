Code.require_file "../helpers/process_commands.exs", __DIR__

defmodule Alchemist.Server.IO do

  @moduledoc false

  use GenServer

  alias Alchemist.Helpers.ProcessCommands

  def start(opts) do
    env = Keyword.get(opts, :env)
    GenServer.start_link(__MODULE__,  env, [])
  end

  def init(env) do
    {:ok, env, 0}
  end

  def handle_info(:timeout, env) do
    ProcessCommands.process(read_line(), env)
    |> IO.write
    {:noreply, env, 0}
  end

  def read_line do
    IO.gets("") |> String.trim_trailing()
  end
end
