Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/api/ping.exs", __DIR__

defmodule Alchemist.API.PingTest do

  use ExUnit.Case, async: true

  alias Alchemist.API.Ping

  test "PING request" do
    assert Ping.request =~ """
    PONG
    END-OF-PING
    """
  end
end
