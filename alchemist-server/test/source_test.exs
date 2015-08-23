Code.require_file "test_helper.exs", __DIR__
Code.require_file "../lib/source.exs", __DIR__

defmodule SourceTest do
  use ExUnit.Case

  import Alchemist.Source

  test "find call for defmodule" do
    context = [context: Elixir, imports: [], aliases: []]
    assert find([nil, :defmodule, context]) =~ "lib/elixir/lib/kernel.ex"
  end

  test "find call for import" do
    context = [context: Elixir, imports: [], aliases: []]
    assert find([nil, :import, context]) =~ "lib/elixir/lib/kernel/special_forms.ex"
  end

  test "find call for create_file with available import" do
    context = [context: Elixir, imports: [Mix.Generator], aliases: []]
    assert find([nil, :create_file, context]) =~ "lib/mix/lib/mix/generator.ex"
  end

  test "find call for MyList.flatten with available aliases" do
    context = [context: Elixir, imports: [], aliases: [{MyList, List}]]
    assert find([MyList, :flatten, context]) =~ "lib/elixir/lib/list.ex"
  end

  test "find call for String module" do
    context = [context: Elixir, imports: [], aliases: []]
    assert find([String, nil, context]) =~ "lib/elixir/lib/string.ex"
  end

  test "find call for erlang module" do
    context = [ context: Elixir, imports: [], aliases: [] ]
    assert find([:lists, :duplicate, context]) =~ "lib/stdlib/src/lists.erl"
  end

  test "find call for none existing module" do
    context = [ context: Elixir, imports: [], aliases: [] ]
    assert find([Rock, :duplicate, context]) == nil
  end

end
