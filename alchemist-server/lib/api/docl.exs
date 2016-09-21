Code.require_file "../helpers/module_info.exs", __DIR__
Code.require_file "../helpers/capture_io.exs", __DIR__
Code.require_file "../helpers/response.exs", __DIR__

defmodule Alchemist.API.Docl do

  @moduledoc false

  import IEx.Helpers, warn: false

  alias Alchemist.Helpers.ModuleInfo
  alias Alchemist.Helpers.CaptureIO
  alias Alchemist.Helpers.Response

  def request(args) do
    args
    |> normalize
    |> process
    |> Response.endmark("DOCL")
  end

  def process([expr, modules, aliases]) do
    search(expr, modules, aliases)
  end

  def search(nil), do: true
  def search(expr) do
    try do
      help = CaptureIO.capture_io(fn ->
        Code.eval_string("h(#{expr})", [], __ENV__)
      end)
      help
    rescue
      _e -> nil
    end
  end

  def search(expr, modules, []) do
    expr = to_string expr
    unless function?(expr) do
      search(expr)
    else
      search_with_context(modules, expr)
    end
  end

  def search(expr, modules, aliases) do
    unless function?(expr) do
      String.split(expr, ".")
      |> ModuleInfo.expand_alias(aliases)
      |> search
    else
      search_with_context(modules, expr)
    end
  end

  defp search_with_context(modules, expr) do
    modules ++ [Kernel, Kernel.SpecialForms]
    |> build_search(expr)
    |> search
  end

  defp build_search(modules, search) do
    function = Regex.replace(~r/\/[0-9]$/, search, "")
    function = String.to_atom(function)
    for module <- modules,
    ModuleInfo.docs?(module, function) do
      "#{module}.#{search}"
    end |> List.first
  end

  defp function?(expr) do
    Regex.match?(~r/^[a-z_]/, expr)
  end

  defp normalize(request) do
    {{expr, [ context: _,
              imports: imports,
              aliases: aliases]}, _} = Code.eval_string(request)
    [expr, imports, aliases]
  end
end
