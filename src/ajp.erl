%%%-------------------------------------------------------------------
%%% File:      ajp.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2008 Jebu Ittiachen
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-13 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-module(ajp).
-author('jebui@yahoo-inc.com').
-export([receive_ajp13_message/1]).

-compile(export_all).

-include_lib("../include/ajp_records.hrl").

receive_ajp13_message(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Binary} -> 
      {ok, AJP_Request} = parse_ajp_message(Binary),
      {ok, AJP_Request};
    _Other -> {error, invalid_ajp_header}
  end.

parse_ajp_message(<< 18,52, AJP_Body/binary >>) ->
  parse_ajp_body(AJP_Body);
parse_ajp_message(_) ->
  {error, "Invalid AJP envelope"}.
  
parse_ajp_body( << RequestLength:16, 2, AJP_ForwardRequest/binary >> ) ->
  parse_ajp_forward_request(AJP_ForwardRequest);
parse_ajp_body(_) ->
  {error, "Unknown AJP request"}.
  
parse_ajp_forward_request(<< 1, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "OPTIONS"});
parse_ajp_forward_request(<< 2, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "GET"});
parse_ajp_forward_request(<< 3, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "HEAD"});
parse_ajp_forward_request(<< 4, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "POST"});
parse_ajp_forward_request(<< 5, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "PUT"});
parse_ajp_forward_request(<< 6, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "DELETE"});
parse_ajp_forward_request(<< 7, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "TRACE"});
parse_ajp_forward_request(<< 8, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "PROPFIND"});
parse_ajp_forward_request(<< 9, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "PROPPATCH"});
parse_ajp_forward_request(<< 10, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "MKCOL"});
parse_ajp_forward_request(<< 11, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "COPY"});
parse_ajp_forward_request(<< 12, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "MOVE"});
parse_ajp_forward_request(<< 13, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "LOCK"});
parse_ajp_forward_request(<< 14, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "UNLOCK"});
parse_ajp_forward_request(<< 15, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "ACL"});
parse_ajp_forward_request(<< 16, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "REPORT"});
parse_ajp_forward_request(<< 17, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "VERSION-CONTROL"});
parse_ajp_forward_request(<< 18, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "CHECKIN"});
parse_ajp_forward_request(<< 19, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "CHECKOUT"});
parse_ajp_forward_request(<< 20, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "UNCHECKOUT"});
parse_ajp_forward_request(<< 21, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "SEARCH"});
parse_ajp_forward_request(<< 22, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "MKWORKSPACE"});
parse_ajp_forward_request(<< 23, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "UPDATE"});
parse_ajp_forward_request(<< 24, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "LABEL"});
parse_ajp_forward_request(<< 25, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "MERGE"});
parse_ajp_forward_request(<< 26, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "BASELINE_CONTROL"});
parse_ajp_forward_request(<< 27, AJP_Request/binary >>) ->
  parse_ajp_request_body(AJP_Request, #ajp_request_envelope{method = "MKACTIVITY"});
parse_ajp_forward_request(Req) ->
  {error, "Unknown request method", Req}.
  
parse_ajp_request_body(AJP_Request, AJP_Request_Envelope) ->
  {Protocol, A1} = parse_ajp_string(AJP_Request),
  {RequestURI, A2} = parse_ajp_string(A1),
  {RemoteAddr, A3} = parse_ajp_string(A2),
  {RemoteHost, A4} = parse_ajp_string(A3),
  {ServerName, A5} = parse_ajp_string(A4),
  <<Port:16, IsSSL:8, HeaderLength:16, A6/binary>> = A5,
  {ok, AJP_Headers, Rest} = decode_encoded_headers(A6, [], HeaderLength),
  % TODO: we are currently not doing the optional attributes and will fail if we get any
  % we let this to not disrupt our life.
  % << 16#FF >> = Rest,
  
  {ok, AJP_Request_Envelope#ajp_request_envelope{
    protocol = Protocol,
    request_uri = RequestURI, 
    remote_address = RemoteAddr, 
    remote_host = RemoteHost, 
    server_name = ServerName, 
    port = Port, 
    is_ssl = IsSSL, 
    headers = AJP_Headers
  }}.

decode_encoded_headers(Rest, HeaderAcc, HeaderLength) when HeaderLength =< 0 ->
  {ok, HeaderAcc, Rest};
decode_encoded_headers(<<AQ, AQ1, Size:16, Name:Size/binary, 0, Rest/binary>>, HeaderAcc, HeaderLength) when AQ == 16#A0 ->
  decode_encoded_headers(Rest, [{lookup_ajp_header_name(AQ1), binary_to_list(Name)} | HeaderAcc], (HeaderLength - 1));
decode_encoded_headers(<<HeaderSize:16, HeaderName:HeaderSize/binary, 0,
                    HeaderVSize:16, HeaderV:HeaderVSize/binary,0,
                    Rest/binary>>, HeaderAcc, HeaderLength) when HeaderSize =< 16#9999 ->
  decode_encoded_headers(Rest, [{binary_to_list(HeaderName), binary_to_list(HeaderV)} | HeaderAcc], (HeaderLength - 1));
decode_encoded_headers(Rest, HeaderAcc, HeaderLength) ->
  {error, "insufficient headers in request", HeaderLength}.
        
lookup_ajp_header_name(16#01) ->
  "accept";
lookup_ajp_header_name(16#02) ->
  "accept-charset";
lookup_ajp_header_name(16#03) ->
  "accept-encoding";
lookup_ajp_header_name(16#04) ->
  "accept-language";
lookup_ajp_header_name(16#05) ->
  "authorization";
lookup_ajp_header_name(16#06) ->
  "connection";
lookup_ajp_header_name(16#07) ->
  "content-type";
lookup_ajp_header_name(16#08) ->
  "content-length";
lookup_ajp_header_name(16#09) ->
  "cookie";
lookup_ajp_header_name(16#0A) ->
  "cookie2";
lookup_ajp_header_name(16#0B) ->
  "host";
lookup_ajp_header_name(16#0C) ->
  "pragma";
lookup_ajp_header_name(16#0D) ->
  "referer";
lookup_ajp_header_name(16#0E) ->
  "user-agent".
  
parse_ajp_string(<< Length:16, AJPString:Length/binary, 0, RestOfBody/binary >>) when Length < 16#FFFF ->
  {binary_to_list(AJPString), RestOfBody};
parse_ajp_string(<< Length:16, Body/binary >>) ->
  {[], Body}.

%% Resonse related 
encode_ajp_body_response(Binary, Length) ->
  << 16#4142:16, (Length + 3):16, 3:8, Length:16, Binary/binary >>.

encode_ajp_header_response(ResponseHeaders) ->
  {ok, NumHeaders, ResponseBytes} = encode_ajp_headers(ResponseHeaders#ajp_response_envelope.headers, 0, <<>>),
  Status = ResponseHeaders#ajp_response_envelope.status,
  Message = encode_ajp_string(ResponseHeaders#ajp_response_envelope.message),
  MessageSize = byte_size(Message),
  ResponseSize = byte_size(ResponseBytes),
  << 16#4142:16,(5 + ResponseSize + MessageSize):16, 4:8, Status:16, Message/binary, NumHeaders:16, ResponseBytes/binary >>.

encode_ajp_end_response() ->
  << 16#4142:16, 2:16, 5:8, 0:8 >>.
  
encode_ajp_get_body_response(Length) ->
  << 16#4142:16, 3:16, 6:8, Length:16 >>.
  
encode_ajp_headers([], HeadersLength, Bytes) ->
  {ok, HeadersLength, Bytes};
encode_ajp_headers([{Name, Val} | Headers], HeadersLength, Bytes) ->
  EName = encode_ajp_header_name(Name),
  EVal = encode_ajp_string(Val),
  encode_ajp_headers(Headers, HeadersLength + 1, << Bytes/binary, EName/binary, EVal/binary >>).
  
encode_ajp_header_name("content-type") ->
  << 16#A001:16 >>;
encode_ajp_header_name("content-language") ->
  << 16#A002:16 >>;
encode_ajp_header_name("content-length") ->
  << 16#A003:16 >>;
encode_ajp_header_name("date") ->
  << 16#A004:16 >>;
encode_ajp_header_name("last-modified") ->
  << 16#A005:16 >>;
encode_ajp_header_name("location") ->
  << 16#A006:16 >>;
encode_ajp_header_name("set-cookie") ->
  << 16#A007:16 >>;
encode_ajp_header_name("set-cookie2") ->
  << 16#A008:16 >>;
encode_ajp_header_name("servlet-engine") ->
  << 16#A009:16 >>;
encode_ajp_header_name("status") ->
  << 16#A00A:16 >>;
encode_ajp_header_name("www-authenticate") ->
  << 16#A00B:16 >>.
  
encode_ajp_string(String) ->
  L = length(String),
  Bytes = list_to_binary(String),
  << L:16, Bytes/binary, 0:8 >>.
  