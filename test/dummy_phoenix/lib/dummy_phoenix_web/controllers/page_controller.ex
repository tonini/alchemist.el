defmodule DummyPhoenixWeb.PageController do
  use DummyPhoenixWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
