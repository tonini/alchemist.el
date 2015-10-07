Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/api/defl.exs", __DIR__

defmodule Alchemist.API.DeflTest do

  use ExUnit.Case

  alias Alchemist.API.Defl

  test "DEFL request call for defmodule" do
    context = [context: Elixir, imports: [], aliases: []]
    assert Defl.process([nil, :defmodule, context]) =~ "lib/elixir/lib/kernel.ex"
  end

  test "DEFL request call for import" do
    context = [context: Elixir, imports: [], aliases: []]
    assert Defl.process([nil, :import, context]) =~ "lib/elixir/lib/kernel/special_forms.ex"
  end

  test "DEFL request call for create_file with available import" do
    context = [context: Elixir, imports: [Mix.Generator], aliases: []]
    assert Defl.process([nil, :create_file, context]) =~ "lib/mix/lib/mix/generator.ex"
  end

  test "DEFL request call for MyList.flatten with available aliases" do
    context = [context: Elixir, imports: [], aliases: [{MyList, List}]]
    assert Defl.process([MyList, :flatten, context]) =~ "lib/elixir/lib/list.ex"
  end

  test "DEFL request call for String module" do
    context = [context: Elixir, imports: [], aliases: []]
    assert Defl.process([String, nil, context]) =~ "lib/elixir/lib/string.ex"
  end

  test "DEFL request call for erlang module" do
    context = [ context: Elixir, imports: [], aliases: [] ]
    assert Defl.process([:lists, :duplicate, context]) =~ "lib/stdlib/src/lists.erl"
  end

  test "DEFL request call for none existing module" do
    context = [ context: Elixir, imports: [], aliases: [] ]
    assert Defl.process([Rock, :duplicate, context]) == nil
  end

end
