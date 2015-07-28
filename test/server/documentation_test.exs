Code.require_file "test_helper.exs", __DIR__
Code.require_file "../../server/documentation.exs", __DIR__

defmodule DocumentationTest do
  use ExUnit.Case

  import Alchemist.Documentation

  test "moduledoc? returns true" do
    assert moduledoc?(List) == true
  end

  test "moduledoc? returns false" do
    assert moduledoc?(List.Chars.Atom) == false
  end

  test "has_doc_for? returns true" do
    assert func_doc?(List, :flatten) == true
    assert func_doc?(Kernel, :def) == true
  end

  test "has_doc_for? returns false" do
    assert func_doc?(List, :dance) == false
    assert func_doc?(nil, :dance) == false
  end

  test "func? returns true" do
    assert func?("flatten") == true
    assert func?("__DIR__") == true
    assert func?("to_string") == true
  end

  test "func? returns false" do
    assert func?("List") == false
    assert func?("String.Unicode") == false
    assert func?("Map.equal?") == false
  end
end
