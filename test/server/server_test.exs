Code.require_file "test_helper.exs", __DIR__
Code.require_file "../../server/server.exs", __DIR__

defmodule ServerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  setup_all do
    on_exit fn ->
      {_status, files} = File.ls "fixtures"
      files |> Enum.each &File.rm "fixtures/#{&1}"
    end
  end

  test "Documentation lookup" do
    assert send_signal("DOC List") =~ """
    Implements functions that only make sense for lists and cannot be part of the
    Enum protocol. In general, favor using the Enum API instead of List.
    """
  end

  test "Empty expression completion" do
    assert send_signal("COMPLETE") =~ """
    cmp:get_and_update_in/2
    cmp:get_and_update_in/3
    cmp:put_in/2
    cmp:put_in/3
    cmp:reraise/2
    cmp:reraise/3
    """
  end

  test "Expression completion" do
    assert send_signal("COMPLETE def") =~ """
    cmp:def
    cmp:defexception/1
    cmp:defoverridable/1
    cmp:defstruct/1
    cmp:def/2
    cmp:defdelegate/2
    cmp:defmacro/2
    cmp:defmacrop/2
    cmp:defmodule/2
    cmp:defp/2
    """
  end

  test "Getting the definition source file information of code" do
    assert send_signal("SOURCE List,delete") =~ "/lib/elixir/lib/list.ex"
  end

  test "Evaluate the content of a file" do
    File.write("fixtures/eval_fixture.exs", "1+1")
    assert send_signal("EVAL fixtures/eval_fixture.exs") =~ "2"
  end

  test "Evaluate and quote the content of a file" do
    File.write("fixtures/eval_and_quote_fixture.exs", "[4,2,1,3] |> Enum.sort")
    assert send_signal("QUOTE fixtures/eval_and_quote_fixture.exs") =~ """
    {{:., [line: 1], [{:__aliases__, [counter: 0, line: 1], [:Enum]}, :sort]},\n   [line: 1], []}]}
    """
  end

  defp send_signal(signal) do
    :ets.new(:alchemist, [:named_table])
    output = capture_io(fn ->
      Alchemist.Server.read_input(signal)
    end)
    :ets.delete_all_objects(:alchemist)
    output
  end

end
