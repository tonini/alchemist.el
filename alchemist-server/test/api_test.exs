Code.require_file "test_helper.exs", __DIR__
Code.require_file "../lib/api/comp.exs", __DIR__
Code.require_file "../lib/api/docl.exs", __DIR__

defmodule APITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API

  test "DOCL request" do
    assert capture_io(fn ->
      API.Docl.process(['defmodule', [], []])
    end) =~ """
    Defines a module given by name with the given contents.
    """
  end

end
