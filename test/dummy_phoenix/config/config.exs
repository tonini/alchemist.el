# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :dummy_phoenix,
  ecto_repos: [DummyPhoenix.Repo]

# Configures the endpoint
config :dummy_phoenix, DummyPhoenixWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "ZwcnLS1bvmFvbtxIC0fOlV/59jxKE5sluxUZmw3zgxNWPv7wtJriT0jjPjqqQKFP",
  render_errors: [view: DummyPhoenixWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: DummyPhoenix.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
