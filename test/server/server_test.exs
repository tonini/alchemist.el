Code.require_file "test_helper.exs", __DIR__
Code.require_file "../../server/server.exs", __DIR__

defmodule ServerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  setup_all do
    on_exit fn ->
      {_status, files} = File.ls Path.expand("fixtures", __DIR__)
      files |> Enum.each fn(file) ->
        unless file == ".gitkeep" do
          File.rm Path.expand("fixtures/#{file}", __DIR__)
        end
      end
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

  test "Expression completion with context" do
    assert send_signal("COMPLETE-WITH-CONTEXT ;[List];[]") =~ """
    cmp:foldl/3
    cmp:foldr/3
    cmp:insert_at/3
    cmp:keydelete/3
    cmp:keyfind/4
    cmp:keymember?/3
    cmp:keyreplace/4
    cmp:keysort/2
    cmp:keystore/4
    cmp:last/1
    cmp:module_info/1
    cmp:replace_at/3
    cmp:to_atom/1
    cmp:to_existing_atom/1
    cmp:to_float/1
    cmp:to_integer/1
    cmp:to_list/1
    cmp:to_string/1
    cmp:to_tuple/1
    cmp:update_at/3
    cmp:wrap/1
    cmp:zip/1
    cmp:
    cmp:clear/0
    cmp:flush/0
    cmp:pwd/0
    cmp:respawn/0
    cmp:cd/1
    """
  end

  test "Getting the definition source file information of code" do
    assert send_signal("SOURCE List,delete") =~ "/lib/elixir/lib/list.ex"
  end

  test "Evaluate the content of a file" do
    filename = Path.expand("fixtures/eval_fixture.exs", __DIR__)
    File.write(filename, "1+1")
    assert send_signal("EVAL #{filename}") =~ "2"
  end

  test "Evaluate and quote the content of a file" do
    filename = Path.expand("fixtures/eval_and_quote_fixture.exs", __DIR__)
    File.write(filename, "[4,2,1,3] |> Enum.sort")
    assert send_signal("QUOTE #{filename}") =~ """
    {{:., [line: 1], [{:__aliases__, [counter: 0, line: 1], [:Enum]}, :sort]},\n   [line: 1], []}]}
    """
  end

  test "Get all mix tasks by name" do
    assert send_signal("MIXTASKS") =~ """
    app.start
    archive
    archive.build
    archive.install
    archive.uninstall
    clean
    cmd
    compile
    """
  end

  defp send_signal(signal) do
    capture_io(fn ->
      Alchemist.Server.read_input(signal)
    end)
  end
end
