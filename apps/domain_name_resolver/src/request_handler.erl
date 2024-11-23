-module(request_handler).
-include("common_records.hrl").
-behaviour(cowboy_handler).


-export([init/2, content_types_accepted/2, allowed_methods/2, resolve_dns_query/2, content_types_provided/2]).

-import(resolver, [run/1]).

init( Req, State ) ->
  {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, resolve_dns_query}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, resolve_dns_query}
  ], Req, State}.

resolve_dns_query(Req, State) ->
  io:fwrite("~nTest~n"),
  io:fwrite("State: ~p~n", [State]),
  io:fwrite("Request: ~p~n", [Req]),
  io:fwrite("Has body? ~p~n", [cowboy_req:has_body(Req)]),

  {ok, ReqBody, _} = cowboy_req:read_body(Req),
  io:fwrite("RequestBody: ~p~n", [ReqBody]),
  RequestBodyDecoded = jsx:decode(ReqBody),
  io:fwrite("RequestBody: ~p~n", [RequestBodyDecoded]),
  Query = maps:get(<<"query">>, RequestBodyDecoded),
  {ok, QuerySummary} = resolve_domain(Query),
  io:fwrite("Ips: ~p~n", [QuerySummary]),

  ResolvedIps = lists:map(fun(Ip) -> iolist_to_binary(Ip) end, QuerySummary#query_summary.resolved_ips),
  QuerySteps = lists:map(fun(QueryStep) ->
    #{<<"dnsQueryName">> => iolist_to_binary(QueryStep#query_step.dns_query_name), <<"nameServerIp">> => iolist_to_binary(QueryStep#query_step.name_server_ip), <<"nameServer">> => iolist_to_binary(QueryStep#query_step.name_server)} end, QuerySummary#query_summary.query_steps),


  Response = #{<<"resolvedIps">> => ResolvedIps, <<"querySteps">> => QuerySteps},


  %% Set response headers
  Headers = #{<<"content-type">> => <<"application/json">>},

  %% Send the response
  {ok, Reply} = cowboy_req:reply(200, Headers, jsx:encode(Response), Req),
  {true, Reply, State}.


resolve_domain(Query) ->
  io:fwrite("Query: ~p~n", [Query]),
  {ok, QuerySummary} = run(binary:bin_to_list(Query)),
  io:fwrite("~n Response: ~p~n", [QuerySummary]),


  io:fwrite("Response: ~p~n", [QuerySummary]),
  {ok, QuerySummary}.