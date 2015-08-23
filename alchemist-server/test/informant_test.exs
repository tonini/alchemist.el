Code.require_file "test_helper.exs", __DIR__
Code.require_file "../lib/informant.exs", __DIR__

defmodule InformantTest do
  use ExUnit.Case

  import Alchemist.Informant

  test "has_function? return true" do
    assert has_function?(List, :flatten) == true
    assert has_function?(List, :to_string) == true
  end

  test "has_function? return false" do
    assert has_function?(List, :split) == false
    assert has_function?(List, :map) == false
  end
end
