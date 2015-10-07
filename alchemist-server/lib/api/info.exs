Code.require_file "../helpers/module_info.exs", __DIR__
Code.require_file "../helpers/complete.exs", __DIR__

defmodule Alchemist.API.Info do

  @moduledoc false

  alias Alchemist.Helpers.ModuleInfo
  alias Alchemist.Helpers.Complete

  def request(args) do
    args
    |> normalize
    |> process
  end

  def process(:modules) do
    modules = ModuleInfo.all_applications_modules
    |> Enum.uniq
    |> Enum.reject(&is_nil/1)
    |> Enum.filter(&ModuleInfo.moduledoc?/1)

    functions = Complete.run('')

    modules ++ functions
    |> Enum.uniq
    |> Enum.map &IO.puts/1

    IO.puts "END-OF-INFO"
  end

  def process(:mixtasks) do
    # append things like hex or phoenix archives to the load_path
    Mix.Local.append_archives

    :code.get_path
    |> Mix.Task.load_tasks
    |> Enum.map(&Mix.Task.task_name/1)
    |> Enum.sort
    |> Enum.map &IO.puts/1

    IO.puts "END-OF-INFO"
  end

  def normalize(request) do
    {{_, name }, _} = Code.eval_string(request)
    name
  end
end
