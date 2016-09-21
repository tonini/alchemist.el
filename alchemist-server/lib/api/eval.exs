Code.require_file "../helpers/response.exs", __DIR__

defmodule Alchemist.API.Eval do

  @moduledoc false

  alias Alchemist.Helpers.CaptureIO
  alias Alchemist.Helpers.Response

  def request(args) do
    args
    |> normalize
    |> process
    |> Response.endmark("EVAL")
  end

  def process({:eval, file}) do
    CaptureIO.capture_io(fn ->
      try do
        File.read!("#{file}")
        |> Code.eval_string
        |> Tuple.to_list
        |> List.first
        |> IO.inspect
      rescue
        e -> IO.inspect e
      end
    end)
  end

  def process({:quote, file}) do
    CaptureIO.capture_io(fn ->
      try do
        File.read!("#{file}")
        |> Code.string_to_quoted
        |> Tuple.to_list
        |> List.last
        |> IO.inspect
      rescue
        e -> IO.inspect e
      end
    end)
  end

  def process({:expand, file}) do
    CaptureIO.capture_io(fn ->
      try do
        {_, expr} = File.read!("#{file}")
                    |> Code.string_to_quoted
        res = Macro.expand(expr, __ENV__)
        IO.puts Macro.to_string(res)
      rescue
        e -> IO.inspect e
      end
    end)
  end

  def process({:expand_once, file}) do
    CaptureIO.capture_io(fn ->
      try do
        {_, expr} = File.read!("#{file}")
                    |> Code.string_to_quoted
        res = Macro.expand_once(expr, __ENV__)
        IO.puts Macro.to_string(res)
      rescue
        e -> IO.inspect e
      end
    end)
  end

  def normalize(request) do
    {expr , _} = Code.eval_string(request)
    expr
  end
end
