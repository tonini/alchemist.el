Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/api/comp.exs", __DIR__
Code.require_file "../../lib/api/docl.exs", __DIR__

defmodule Alchemist.API.DoclTest do

  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API.Docl

  test "DOCL request" do
    assert capture_io(fn ->
      Docl.process(['defmodule', [], []])
    end) =~ """
    Defines a module given by name with the given contents.
    """
  end

  test "DOCL request for List.flatten" do
    assert capture_io(fn ->
      Docl.process(["List.flatten", [], []])
    end) =~ """
    Flattens the given \e[36mlist\e[0m of nested lists.
    \e[0m
    \e[33mExamples\e[0m
    \e[0m
    \e[36m\e[1m┃ iex> List.flatten([1, [[2], 3]])
    """
  end

  test "DOCL request for MyCustomList.flatten with alias" do
    assert capture_io(fn ->
      Docl.process(["MyCustomList.flatten", [], [{MyCustomList, List}]])
    end) =~ """
    Flattens the given \e[36mlist\e[0m of nested lists.
    \e[0m
    \e[33mExamples\e[0m
    \e[0m
    \e[36m\e[1m┃ iex> List.flatten([1, [[2], 3]])
    """
  end

  test "DOCL request for search create_file with import" do
    assert capture_io(fn ->
      Docl.process(["create_file", [Mix.Generator], []])
    end) =~ """
    def create_file(path, contents, opts \\\\ [])                   \e[0m
    \e[0m
    Creates a file with the given contents. If the file already exists, asks for
    user confirmation.
    \e[0m
    """
  end

  test "DOCL request for defmacro" do
    assert capture_io(fn ->
      Docl.process(["defmacro", [], []])
    end) =~ """
    \e[7m\e[33m                      defmacro defmacro(call, expr \\\\ nil)                      \e[0m
    """
  end

  test "DOCL request for Path.basename/1" do
    assert capture_io(fn ->
      Docl.process(["Path.basename/1", [], []])
    end) =~ """
    Returns the last component of the path or the path itself if it does not
    contain any directory separators.
    """
  end

end
