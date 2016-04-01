## Server

Alchemist works as a server & client model, where the Alchemist server is written in Elixir and Emacs takes the part of the client.

For each Elixir mix project Alchemist starts an independent server in the background. If an Elixir file gets opened in a buffer,
Alchemist checks if a server is running for the current project or not and starts a new one if needed. If there is no Elixir mix project found, Alchemist
starts a general unique server.

An Alchemist server can be started/restarted by hand with the call of the function `alchemist-server-start`.
A prompt for the environment in which the server should run (default: `dev`) gets opened and the server will start/restart in the selected environment.

To get the server status for the current project, just call the function `alchemist-server-status` and a report will be displayed in the minibuffer:

```
Alchemist-Server-Status: [Project: /Users/tonini/Projects/ek/ Status: Connected]
```
