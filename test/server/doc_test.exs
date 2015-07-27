Code.require_file "test_helper.exs", __DIR__
Code.require_file "../../server/doc.exs", __DIR__

defmodule DocTest do
  use ExUnit.Case

  test "moduledoc? returns true" do
    assert Alchemist.Doc.moduledoc?(List) == true
  end

  test "moduledoc? returns false" do
    assert Alchemist.Doc.moduledoc?(List.Chars.Atom) == false
  end
end
