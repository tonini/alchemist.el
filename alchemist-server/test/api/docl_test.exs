Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/api/comp.exs", __DIR__
Code.require_file "../../lib/api/docl.exs", __DIR__

defmodule Alchemist.API.DoclTest do

  use ExUnit.Case, async: true

  alias Alchemist.API.Docl

  test "DOCL full request" do
    docl_resp = Docl.request(~s({ "defmodule", [ context: Elixir, imports: [], aliases: [] ] }))
    assert docl_resp =~ """
    Defines a module given by name with the given contents.
    """
    assert docl_resp =~ ~r"END-OF-DOCL$"
  end

  test "DOCL request" do
    assert Docl.process(['defmodule', [], []]) =~ """
    Defines a module given by name with the given contents.
    """
  end

  test "DOCL request with no match" do
    assert Docl.process(['FooBar', [], []]) =~ "Could not load module FooBar"
  end


  test "DOCL request for List.flatten" do
    assert Docl.process(["List.flatten", [], []]) =~ """
    Flattens the given \e[36mlist\e[0m of nested lists.
    """
  end

  test "DOCL request for MyCustomList.flatten with alias" do
    assert Docl.process(["MyCustomList.flatten", [], [{MyCustomList, List}]]) =~ """
    Flattens the given \e[36mlist\e[0m of nested lists.
    """
  end

  test "DOCL request for search create_file with import" do
    assert Docl.process(["create_file", [Mix.Generator], []]) =~ """
    def create_file(path, contents, opts \\\\ [])                   \e[0m
    \e[0m
    Creates a file with the given contents. If the file already exists, asks for
    user confirmation.
    \e[0m
    """
  end

  test "DOCL request for defmacro" do
    assert Docl.process(["defmacro", [], []]) =~ """
    \e[7m\e[33m                      defmacro defmacro(call, expr \\\\ nil)                      \e[0m
    """
  end

  test "DOCL request for Path.basename/1" do
    assert Docl.process(["Path.basename/1", [], []]) =~ """
    Returns the last component of the path or the path itself if it does not
    contain any directory separators.
    """
  end

end
