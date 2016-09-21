defmodule Alchemist.Helpers.Response do

  @moduledoc false

  def endmark(nil, cmd) do
    "END-OF-#{cmd}\n"
  end

  def endmark("", cmd) do
    "END-OF-#{cmd}\n"
  end

  def endmark(response, cmd) do
    response = String.strip(response)
    "#{response}\nEND-OF-#{cmd}\n"
  end
end
