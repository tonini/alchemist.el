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

  test "has_doc_for? returns true" do
    assert Alchemist.Doc.func_doc?(List, :flatten) == true
  end

  test "has_doc_for? returns false" do
    assert Alchemist.Doc.func_doc?(List, :dance) == false
  end
end
