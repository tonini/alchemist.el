Code.require_file "test_helper.exs", __DIR__
Code.require_file "../lib/documentation.exs", __DIR__

defmodule DocumentationTest do
  use ExUnit.Case

  import Alchemist.Documentation

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
