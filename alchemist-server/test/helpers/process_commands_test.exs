Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/helpers/process_commands.exs", __DIR__

defmodule ServerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  setup_all do
    on_exit fn ->
      {_status, files} = File.ls Path.expand("../fixtures", __DIR__)
      files |> Enum.each(fn(file) ->
        unless file == ".gitkeep" do
          File.rm Path.expand("../fixtures/#{file}", __DIR__)
        end
      end)
    end
  end

  test "Expression completion" do
    assert send_signal("COMP { 'def', [context: Elixir, imports: [], aliases: []]}") =~ """
    defoverridable/1
    """
  end

  test "Documentation lookup" do
    assert send_signal("DOCL { 'List', [context: Elixir, imports: [], aliases: []]}") =~ """
    \e[0m\n\e[7m\e[33m                                      List                                      \e[0m\n\e[0m
    """
  end

  test "Getting the definition source file information of code" do
    assert send_signal("DEFL {\"List,delete\", [context: Elixir, imports: [], aliases: []]}") =~ "/lib/elixir/lib/list.ex"
  end

  test "Evaluate the content of a file" do
    filename = Path.expand("../fixtures/eval_fixture.exs", __DIR__)
    File.write(filename, "1+1")
    assert send_signal("EVAL { :eval, '#{filename}' }") =~ "2"
  end

  test "Evaluate and quote the content of a file" do
    filename = Path.expand("../fixtures/eval_and_quote_fixture.exs", __DIR__)
    File.write(filename, "[4,2,1,3] |> Enum.sort")
    assert send_signal("EVAL { :quote, '#{filename}' }") =~ """
    {{:., [line: 1], [{:__aliases__, [counter: 0, line: 1], [:Enum]}, :sort]},\n   [line: 1], []}]}
    """
  end

  test "Expand macro once" do
    filename = Path.expand("../fixtures/macro_expand_once_fixture.exs", __DIR__)
    File.write(filename, "unless true, do: IO.puts \"this should never be printed\"")
    assert send_signal("EVAL { :expand_once, '#{filename}' }") =~ """
    if(true) do
      nil
    else
      IO.puts("this should never be printed")
    end
    """
  end

  test "Expand macro" do
    filename = Path.expand("../fixtures/macro_expand_fixture.exs", __DIR__)
    File.write(filename, "unless true, do: IO.puts \"this should never be printed\"")
    assert send_signal("EVAL { :expand, '#{filename}' }") =~ """
    case(true) do
      x when x in [false, nil] ->
        IO.puts("this should never be printed")
      _ ->
        nil
    end
    """
  end

  test "Get all available application modules" do
    assert send_signal("INFO { :type, :modules }") =~ """
    Elixir.Logger
    Elixir.Logger.Formatter
    Elixir.Logger.Translator
    """
  end

  test "Get all available mix tasks by name" do
    assert send_signal("INFO { :type, :mixtasks }") =~ """
    clean
    cmd
    compile
    """
  end

  # The IEx.Helpers.t and IEx.Helpers.i are functionality which come with
  # Elixir version 1.2.0
  if Version.match?(System.version, ">=1.2.0-rc") do
    test "Get information from data type" do
      assert send_signal("INFO { :type, :info, List}") =~ """
      END-OF-INFO
      """
    end

    test "Don't crash server if data type argument is faulty" do
      assert send_signal("INFO { :type, :info, whatever()}") =~ """
      END-OF-INFO
      """
    end

    test "Prints the types for the given module or for the given function/arity pair" do
      assert send_signal("INFO { :type, :types, 'Agent'}") =~ """
      @type agent() :: pid() | {atom(), node()} | name()\e[0m\n\e[22m@type state() :: term()\e[0m\nEND-OF-INFO
      """

      assert send_signal("INFO { :type, :types, 'Agent.on_start/0'}") =~ """
      @type on_start() :: {:ok, pid()} | {:error, {:already_started, pid()} | term()}\e[0m
      """
    end
  end

  defp send_signal(signal) do
    capture_io(fn ->
      Alchemist.Helpers.ProcessCommands.process(signal, "env") |> IO.write
    end)
  end
end
