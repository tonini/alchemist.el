Code.require_file "test_helper.exs", __DIR__
Code.require_file "../lib/documentation.exs", __DIR__

defmodule DocumentationTest do
  use ExUnit.Case

  import ExUnit.CaptureIO
  import Alchemist.Documentation

  test "search List.flatten" do
    assert capture_io(fn ->
      search("List.flatten")
    end) =~ """
    Flattens the given \e[36mlist\e[0m of nested lists.
    \e[0m
    \e[33mExamples\e[0m
    \e[0m
    \e[36m\e[1m┃ iex> List.flatten([1, [[2], 3]])
    """
  end

  test "search MyCustomList.flatten" do
    assert capture_io(fn ->
      search("MyCustomList.flatten", [], [{MyCustomList, List}])
    end) =~ """
    Flattens the given \e[36mlist\e[0m of nested lists.
    \e[0m
    \e[33mExamples\e[0m
    \e[0m
    \e[36m\e[1m┃ iex> List.flatten([1, [[2], 3]])
    """
  end

  test "search create_file" do
    assert capture_io(fn ->
      search("create_file", [Mix.Generator], [])
    end) =~ """
    def create_file(path, contents, opts \\\\ [])                   \e[0m
    \e[0m
    Creates a file with the given contents. If the file already exists, asks for
    user confirmation.
    \e[0m
    """
  end

  test "search defmacro" do
    assert capture_io(fn ->
      search("defmacro", [], [])
    end) =~ """
    Defines a macro with the given name and contents.
    """
  end

  test "search Path.basename/1" do
    assert capture_io(fn ->
      search("Path.basename/1", [], [])
    end) =~ """
    Returns the last component of the path or the path itself if it does not
    contain any directory separators.
    """
  end

  test "moduledoc? returns true" do
    assert moduledoc?(List) == true
  end

  test "moduledoc? returns false" do
    assert moduledoc?(List.Chars.Atom) == false
  end

  test "func_doc? returns true" do
    assert func_doc?(List, :flatten) == true
    assert func_doc?(Kernel, :def) == true
  end

  test "func_doc? returns false" do
    assert func_doc?(List, :dance) == false
    assert func_doc?(nil, :dance) == false
  end
end
