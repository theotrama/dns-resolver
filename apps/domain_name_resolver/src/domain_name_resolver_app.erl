%%%-------------------------------------------------------------------
%% @doc hello_server public API
%% @end
%%%-------------------------------------------------------------------

-module(domain_name_resolver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/dns", request_handler, [create]},
      {"/static", cowboy_static, {priv_file, domain_name_resolver, "static/index.html"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(
    hello_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),
  domain_name_resolver_sup:start_link().

stop(_State) ->
  ok.
