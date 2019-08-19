Code.require_file "../helpers/process_commands.exs", __DIR__

defmodule Alchemist.Server.Socket do

  alias Alchemist.Helpers.ProcessCommands

  def start(opts) do
    import Supervisor.Spec

    env = Keyword.get(opts, :env)
    port = Keyword.get(opts, :port, 0)

    children = [
      supervisor(Task.Supervisor, [[name: Alchemist.Server.Socket.TaskSupervisor]]),
      worker(Task, [__MODULE__, :accept, [env, port]])
    ]

    opts = [strategy: :one_for_one, name: Alchemist.Server.Socket.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def accept(env, port) do
    {:ok, socket} = :gen_tcp.listen(port,
                    [:binary, packet: :line, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(socket)
    IO.puts "ok|localhost:#{port}"
    loop_acceptor(socket, env)
  end

  defp loop_acceptor(socket, env) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = Task.Supervisor.start_child(Alchemist.Server.Socket.TaskSupervisor, fn -> serve(client, env) end)
    :ok = :gen_tcp.controlling_process(client, pid)
    loop_acceptor(socket, env)
  end

  defp serve(socket, env) do
    case read_line(socket) do
      :closed -> {:stop, :closed}
      data when is_binary(data) ->

        data
        |> String.trim
        |> ProcessCommands.process(env)
        |> write_line(socket)

        serve(socket, env)
    end
  end

  defp read_line(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} -> data
      {:error, :closed} -> :closed
    end
  end

  defp write_line(line, socket) do
    :gen_tcp.send(socket, line)
  end
end
