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
      {"/", cowboy_static, {priv_file, domain_name_resolver, "static/index.html"}},
      {"/favicon.ico", cowboy_static, {priv_file, domain_name_resolver, "static/favicon.ico"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(
    domain_name_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),
  domain_name_resolver_sup:start_link().

stop(_State) ->
  ok.
