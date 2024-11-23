%%%-------------------------------------------------------------------
%%% @author jankoch
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Feb 2023 10:53
%%%-------------------------------------------------------------------
-module(resolver).
-include("common_records.hrl").
-author("jankoch").

%% API
-export([run/1]).
-import(string, [tokens/2, concat/2]).


run(Domain) ->
  {ok, ResultRequestResponses} = resolve(Domain, "199.7.83.42", "l.root-servers.net"),
  io:fwrite("~n Result List: ~p~n", [ResultRequestResponses]),


  DnsResponse = lists:last(ResultRequestResponses),
  io:fwrite("~n Response: ~p~n", [DnsResponse]),


  AnswerRecords = DnsResponse#request_response.dns_response#dns_response.answer_records,

  io:format("~n~n---------ALL QUERIES---------~n"),
  lists:foreach(fun(RequestResponse) ->
    io:format("~p~n", [RequestResponse#request_response.dns_request]) end, ResultRequestResponses),

  DnsRequests = lists:map(fun(RequestResponse) ->
    RequestResponse#request_response.dns_request end, ResultRequestResponses),
  io:fwrite("~nAll DNS requests: ~p~n", [DnsRequests]),

  DnsResponses = lists:map(fun(RequestResponse) ->
    RequestResponse#request_response.dns_response end, ResultRequestResponses),
  io:fwrite("~nAll DNS responses: ~p~n", [DnsResponses]),

  NameServerIps = lists:map(fun(RequestResponse) ->
    RequestResponse#request_response.name_server_ip end, ResultRequestResponses),
  io:fwrite("~nAll name server IPs: ~p~n", [NameServerIps]),


  NameServers = lists:map(fun(RequestResponse) ->
    RequestResponse#request_response.name_server end, ResultRequestResponses),
  io:fwrite("~nAll name servers: ~p~n", [NameServers]),


  io:format("~n~n---------ALL RESPONSES---------~n"),
  lists:foreach(fun(RequestResponse) ->
    io:format("~p~n", [RequestResponse#request_response.dns_response]) end, ResultRequestResponses),

  io:format("~n~n---------ANSWER---------~n"),
  lists:foreach(fun(AnswerRecord) ->
    io:format("Domain: ~p IPv4: ~p~n", [string:join(AnswerRecord#additional_record.name, "."), AnswerRecord#additional_record.ip])
                end, AnswerRecords),


  QuerySteps = lists:map(fun(RequestResponse) ->
    #query_step{dns_query_name = RequestResponse#request_response.dns_request#dns_request.query_section#query_section.qname,
      name_server_ip = RequestResponse#request_response.name_server_ip, name_server = RequestResponse#request_response.name_server} end, ResultRequestResponses),
  io:fwrite("~nNice: ~p~n", [QuerySteps]),

  ResolvedIps = lists:map(fun(AnswerRecord) -> AnswerRecord#additional_record.ip
                          end, AnswerRecords),
  io:format("~p~n", [ResolvedIps]),
  QuerySummary = #query_summary{query_steps = QuerySteps, resolved_ips = ResolvedIps},

  {ok, QuerySummary}.

resolve(Domain, DnsServerIp, DnsServerName) ->
  {ok, DnsRequest} = build_dns_request(Domain),
  {ok, DnsResponseUnparsed} = send_dns_request(DnsRequest, DnsServerIp, 53),
  {ok, DnsResponse} = parse_dns_response(DnsResponseUnparsed),

  if DnsResponse#dns_response.answer_type == 1 ->
    io:fwrite("~nAnswer found: ~p~n", [DnsResponse#dns_response.answer_records]),
    io:fwrite("~nAnswer found: ~p~n", [DnsResponse]),
    io:fwrite("~nAnswer found: ~p~n", [DnsRequest]),
    RequestResponse = #request_response{dns_request = DnsRequest, dns_response = DnsResponse, name_server_ip = DnsServerIp, name_server = DnsServerName},

    io:fwrite("~nRequestResponse: ~p~n", [RequestResponse]),
    {ok, [RequestResponse]};
    length(DnsResponse#dns_response.additional_records) == 0 ->
      {ok, ParsedNameserver} = extract_name_server(DnsResponse),

      {ok, NewList} = resolve(ParsedNameserver, "199.7.83.42", "l.root-servers.net"),
      DownstreamDnsResponse = lists:last(NewList),
      UpdatedRequestResponse = [#request_response{dns_request = DnsRequest, dns_response = DnsResponse, name_server_ip = DnsServerIp, name_server = DnsServerName}] ++ NewList,
      AnswerRecords = DownstreamDnsResponse#request_response.dns_response#dns_response.answer_records,

      FirstAnswer = hd(lists:filter(fun(AnswerRecord) ->
        AnswerRecord#additional_record.type == 1 end, AnswerRecords)),
      NewDnsServerIp = FirstAnswer#additional_record.ip,
      NewDnsServerName = string:join(FirstAnswer#additional_record.name, "."),
      io:format("NewDnsServerName: ~p~n", [NewDnsServerName]),

      {ok, NewRequestResponse} = resolve(Domain, NewDnsServerIp, NewDnsServerName),
      SecondUpdatedList = UpdatedRequestResponse ++ NewRequestResponse,

      {ok, SecondUpdatedList};

    true ->
      {ok, OtherDnsServerIp} = extract_additional_resources(DnsResponse),

      AuthorityRecord = hd(DnsResponse#dns_response.authority_records),
      NewDnsServerName = string:join(AuthorityRecord#authority_record.nameserver_name, "."),

      {ok, List} = resolve(Domain, OtherDnsServerIp, NewDnsServerName),
      UpdatedList = [#request_response{dns_request = DnsRequest, dns_response = DnsResponse, name_server_ip = DnsServerIp, name_server = DnsServerName}] ++ List,
      {ok, UpdatedList}
  end.

extract_additional_resources(DnsResponse) ->
  AdditionalRecords = DnsResponse#dns_response.additional_records,
  FirstAdditionalRecord = hd(lists:filter(fun(AdditionalRecord) ->
    AdditionalRecord#additional_record.type == 1 end, AdditionalRecords)),
  OtherDnsServerIp = FirstAdditionalRecord#additional_record.ip,
  {ok, OtherDnsServerIp}.

extract_name_server(DnsResponse) ->
  AuthorityRecords = DnsResponse#dns_response.authority_records,
  FirstAuthorityRecord = hd(AuthorityRecords),
  Nameserver = FirstAuthorityRecord#authority_record.nameserver_name,
  ParsedNameserver = string:join(Nameserver, "."),
  {ok, ParsedNameserver}.


build_dns_request(DomainName) ->
  Header = #header{transaction_id = 0, qr = 0, opcode = 0, aa = 0, tc = 0, rd = 0, ra = 0, z = 0, rcode = 0, qdcount = 1, ancount = 0, nscount = 0, arcount = 0},
  QuerySection = #query_section{qname = DomainName, qtype = 1, qclass = 1},
  DnsRequest = #dns_request{header = Header, query_section = QuerySection},

  {ok, DnsRequest}.


send_dns_request(Request, Ip, Port) ->
  io:format("~n~n---------DNS REQUEST---------~n"),
  io:format("IP: ~s~n", [Ip]),
  io:format("Port: ~p~n", [Port]),
  {ok, Socket} = gen_udp:open(0, [binary]),
  DnsRequest = serialize_dns_request(Request),
  ok = gen_udp:send(Socket, Ip, Port, binary:bin_to_list(DnsRequest)),
  Value = receive
            {udp, Socket, _, _, Bin} ->
              {ok, Bin}
          after 2000 ->
      error
          end,
  gen_udp:close(Socket),
  Value.


parse_dns_response(DnsResponse) ->
  {ok, {_, _, _, AA, _, _, _, _, _, QueryCount, AnswerRecordCount, NameserverCount, AdditionalRecordCount}} = parse_header_section(DnsResponse),
  {ok, RemainingDnsResponse, {QueryName, Type, Class}} = parse_query_records(DnsResponse, QueryCount),
  io:format("~n~n---------QUERY SECTION---------~n"),
  io:fwrite("Name:  ~p~n", [QueryName]),
  io:fwrite("Type:  ~p~n", [Type]),
  io:fwrite("Class: ~p~n", [Class]),


  io:format("~n~n---------ANSWER RECORD SECTION---------~n"),
  {ok, RemainingDnsResponse2, AnswerRecords} = parse_additional_records(DnsResponse, RemainingDnsResponse, AnswerRecordCount),
  io:fwrite("~p~n~n", [AnswerRecords]),


  io:format("~n~n---------AUTHORITY RECORD SECTION---------~n"),
  {ok, RemainingDnsResponse3, AuthorityRecords} = parse_authority_records(DnsResponse, RemainingDnsResponse2, NameserverCount),
  io:fwrite("~p~n", [AuthorityRecords]),


  io:format("~n~n---------ADDITIONAL RECORD SECTION---------~n"),
  {ok, _, AdditionalRecords} = parse_additional_records(DnsResponse, RemainingDnsResponse3, AdditionalRecordCount),
  io:fwrite("~p~n", [AdditionalRecords]),


  ParsedDnsResponse = #dns_response{answer_type = AA, authority_records = AuthorityRecords, additional_records = AdditionalRecords, answer_records = AnswerRecords},
  {ok, ParsedDnsResponse}.

parse_header_section(Response) ->
  io:format("~n~n---------HEADER---------~n"),
  <<ID:16, QR:1, Opcode:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, _/binary>> = Response,
  io:fwrite("Questions:     ~p~n", [QDCOUNT]),
  io:fwrite("Answer RRs:    ~p~n", [ANCOUNT]),
  io:fwrite("Authority RRs: ~p~n", [NSCOUNT]),
  io:fwrite("Additonal RRs: ~p~n", [ARCOUNT]),
  {ok, {ID, QR, Opcode, AA, TC, RD, RA, Z, RCODE, QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT}}.

parse_additional_records(_, RemainingDnsResponse, 0) ->
  {ok, RemainingDnsResponse, []};
parse_additional_records(DnsResponse, RemainingDnsResponse, AdditionalRecordCount) ->
  {ok, Name, NewRemainingDnsResponse} = parse_name(DnsResponse, RemainingDnsResponse),
  <<Type:16, Class:16, Ttl:32, DataLength:16, RemainingDnsResponse2/binary>> = NewRemainingDnsResponse,

  io:fwrite("Type: ~p~n", [Type]),
  DataLengthBytes = DataLength,
  <<Data:DataLengthBytes/binary, RemainingDnsResponse3/binary>> = RemainingDnsResponse2,
  Ip = inet:ntoa(list_to_tuple(binary_to_list(Data))),
  AdditionalRecord = #additional_record{name = Name, type = Type, class = Class, ttl = Ttl, data_length = DataLength, ip = Ip},

  {ok, RemainingDnsResponse4, NewAdditionalRecord} = parse_additional_records(DnsResponse, RemainingDnsResponse3, AdditionalRecordCount - 1),

  FullList = lists:append([AdditionalRecord], NewAdditionalRecord),
  {ok, RemainingDnsResponse4, FullList}.

parse_authority_records(_, RemainingDnsResponse, 0) ->
  {ok, RemainingDnsResponse, []};
parse_authority_records(DnsResponse, RemainingDnsResponse, AuthorityRecordCount) ->
  {ok, Name, NewRemainingDnsResponse} = parse_name(DnsResponse, RemainingDnsResponse),
  <<Type:16, Class:16, Ttl:32, DataLength:16, RemainingDnsResponse2/binary>> = NewRemainingDnsResponse,
  DataLengthBytes = DataLength * 8,
  <<_:DataLengthBytes, RemainingDnsResponse3/binary>> = RemainingDnsResponse2,

  {ok, NameserverName, _} = parse_name(DnsResponse, RemainingDnsResponse2),

  AuthorityRecord = #authority_record{name = Name, type = Type, class = Class, ttl = Ttl, data_length = DataLength, nameserver_name = NameserverName},

  {ok, RemainingDnsResponse4, NewAuthorityRecord} = parse_authority_records(DnsResponse, RemainingDnsResponse3, AuthorityRecordCount - 1),

  FullList = lists:append([AuthorityRecord], NewAuthorityRecord),
  {ok, RemainingDnsResponse4, FullList}.

parse_query_records(DnsResponse, 0) ->
  {ok, DnsResponse};
parse_query_records(DnsResponse, QueryCount) ->
  <<_:96, ResourceRecords/binary>> = DnsResponse,
  {ok, FullName, RemainingDnsResponse} = parse_name(DnsResponse, ResourceRecords),
  io:fwrite("Full name is: ~p~n", [FullName]),

  <<Type:16, Class:16, NewRemainingDnsResponse/binary>> = RemainingDnsResponse,
  io:fwrite("Type is: ~p~n", [Type]),
  io:fwrite("Class is: ~p~n", [Class]),
  parse_query_records(DnsResponse, QueryCount - 1),
  {ok, NewRemainingDnsResponse, {FullName, Type, Class}}.


parse_name(DnsResponse, RemainingDnsResponse) ->
  <<Length:8, Remainder/binary>> = RemainingDnsResponse,
  case Length of
    0 ->
      <<_:8, NewRemainingDnsResponse/binary>> = RemainingDnsResponse,
      {ok, "", NewRemainingDnsResponse};
    Length when Length == 192; Length == 193 ->
      <<_:8, Offset:8, _/binary>> = RemainingDnsResponse,
      <<_:Offset/binary, OffsetResponse/binary>> = DnsResponse,
      {ok, Name, _} = parse_name(DnsResponse, OffsetResponse),
      <<_:16, NewRemainingDnsResponse/binary>> = RemainingDnsResponse,
      {ok, Name, NewRemainingDnsResponse};
    _ ->
      <<NamePart:Length/binary, Remainder2/binary>> = Remainder,
      {ok, NamePart2, NewRemainingDnsResponse} = parse_name(DnsResponse, Remainder2),
      FullName = lists:append([binary_to_list(NamePart)], NamePart2),
      {ok, FullName, NewRemainingDnsResponse}
  end.

serialize_header(#header{transaction_id = TransactionId, qr = Qr, opcode = Opcode,
  aa = Aa, tc = Tc, rd = Rd, ra = Ra, z = Z, rcode = Rcode,
  qdcount = Qdcount, ancount = Ancount, nscount = Nscount, arcount = Arcount}) ->
  <<TransactionId:16,
    Qr:1,
    Opcode:4,
    Aa:1,
    Tc:1,
    Rd:1,
    Ra:1,
    Z:3,
    Rcode:4,
    Qdcount:16,
    Ancount:16,
    Nscount:16,
    Arcount:16>>.

serialize_query_section(#query_section{qname = QName, qtype = QType, qclass = QClass}) ->
  QueryList = string:tokens(QName, "."),

  QueryListTransformed = lists:map(fun(X) -> [length(X), lists:map(fun(Y) -> [Y] end, X)] end, QueryList),
  QueryListFlattened = lists:flatten(QueryListTransformed),
  QueryListZeroTerminated = QueryListFlattened ++ [0],


  Test = binary:list_to_bin(QueryListZeroTerminated),
  QTypeBinary = <<QType:16/unit:1>>,
  QClassBinary = <<QClass:16/unit:1>>,
  <<Test/binary, QTypeBinary/binary, QClassBinary/binary>>.

serialize_dns_request(#dns_request{header = Header, query_section = QuerySection}) ->
  HeaderBinary = serialize_header(Header),
  QuerySectionBinary = serialize_query_section(QuerySection),
  <<HeaderBinary/binary, QuerySectionBinary/binary>>.
