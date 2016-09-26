Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/api/comp.exs", __DIR__

defmodule Alchemist.API.CompTest do

  use ExUnit.Case, async: true

  alias Alchemist.API.Comp

  test "COMP request with empty hint" do
    assert Comp.process([nil, Elixir, [], [] ]) =~ """
    import/2
    quote/2
    require/2
    END-OF-COMP
    """
  end

  test "COMP request without empty hint" do
    assert Comp.process(['is_b', Elixir, [], []]) =~ """
    is_b
    is_binary/1
    is_bitstring/1
    is_boolean/1
    END-OF-COMP
    """
  end

  test "COMP request with an alias" do
    assert Comp.process(['MyList.flat', Elixir, [], [{MyList, List}]]) =~ """
    MyList.flatten
    flatten/1
    flatten/2
    END-OF-COMP
    """
  end

  test "COMP request with a module hint" do
    assert Comp.process(['Str', Elixir, [], []]) =~ """
    Str
    Stream
    String
    StringIO
    END-OF-COMP
    """
  end

  test "return redundant candidates with COMP" do
    assert Comp.process(['Enu', Elixir, [], []]) =~ """
    Enum
    Enum
    Enumerable
    END-OF-COMP
    """
  end

  test "COMP request with no match" do
    assert Comp.process(['Fooo', Elixir, [], []]) == "END-OF-COMP\n"
  end

end
