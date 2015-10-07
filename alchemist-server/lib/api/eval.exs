defmodule Alchemist.API.Eval do

  @moduledoc false

  def request(args) do
    args
    |> normalize
    |> process
  end

  def process({:eval, file}) do
    try do
      File.read!("#{file}")
      |> Code.eval_string
      |> Tuple.to_list
      |> List.first
      |> IO.inspect
    rescue
      e -> IO.inspect e
    end

    IO.puts "END-OF-EVAL"
  end

  def process({:quote, file}) do
    try do
      File.read!("#{file}")
      |> Code.string_to_quoted
      |> Tuple.to_list
      |> List.last
      |> IO.inspect
    rescue
      e -> IO.inspect e
    end

    IO.puts "END-OF-QUOTE"
  end

  def normalize(request) do
    {expr , _} = Code.eval_string(request)
    expr
  end
end
