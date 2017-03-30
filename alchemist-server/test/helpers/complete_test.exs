Code.require_file "../test_helper.exs", __DIR__
Code.require_file "../../lib/helpers/complete.exs", __DIR__

defmodule CompleteTest do
  use ExUnit.Case, async: true

  import Alchemist.Helpers.Complete

  defmodule MyModule do
    def say_hi, do: true
  end

  test "return completion candidates for 'List'" do
    assert run('List') == ['List.', 'Chars', 'first/1', 'last/1', 'to_atom/1',
                           'to_existing_atom/1', 'to_float/1', 'to_string/1', 'to_tuple/1',
                           'wrap/1', 'zip/1', 'delete/2', 'delete_at/2', 'duplicate/2',
                           'keysort/2', 'myers_difference/2', 'flatten/1', 'flatten/2', 'to_integer/1',
                           'to_integer/2', 'foldl/3', 'foldr/3', 'insert_at/3', 'keydelete/3',
                           'keymember?/3', 'keytake/3', 'pop_at/3', 'replace_at/3', 'update_at/3',
                           'keyfind/4', 'keyreplace/4', 'keystore/4']
  end

  test "return completion candidates for 'Str'" do
    assert run('Str') == ['Str', 'Stream', 'String', 'StringIO']
  end

  test "return completion candidates for 'List.del'" do
    assert run('List.del') == ['List.delete', 'delete/2', 'delete_at/2']
  end

  test "return completion candidates for module with alias" do
    Application.put_env(:"alchemist.el", :aliases, [{MyList, List}])

    assert run('MyList.del') == ['MyList.delete', 'delete/2', 'delete_at/2']
  end

  test "return completion candidates for functions from import" do
    imports = [MyModule]
    assert run('say', imports) == ["say_hi/0"]
  end
end
