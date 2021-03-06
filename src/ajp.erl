%%%-------------------------------------------------------------------
%%% File:      ajp.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2009 Jebu Ittiachen
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-13 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-module(ajp).
-author('jebu@jebu.net').
-export([read_ajp_packet/1, read_ajp_packet/2]).
-export([parse_body/1]).
-export([encode_body_response/2, encode_header_response/1, encode_end_response/0]).
-export([encode_get_body_response/1, encode_headers/3, encode_header_name/1]).
-export([encode_string/1]).
-export([read_buffered_ajp_packet/1, receive_buffered_message/2]).
-include("ajp_records.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
read_ajp_packet(Socket) ->
  read_ajp_packet(Socket, infinity).

read_ajp_packet(Socket, Timeout) ->
  case gen_tcp:recv(Socket, 4, Timeout) of
    {ok, <<18,52, Length:16>>} ->
      {ok, Length, <<18,52, Length:16>>};
    {error, Reason } ->
      {error, Reason}
  end.

read_buffered_ajp_packet(<<18, 52, _:16, 18, 52, Rest/binary>>) ->
  read_buffered_ajp_packet(<<18, 52, Rest/binary>>);
read_buffered_ajp_packet(<<18, 52, L:16, Req:L/binary, Rest/binary>>) ->
  {ok, L, Req, Rest};
read_buffered_ajp_packet(_) ->
  incomplete_packet.

receive_buffered_message(Length, Buffer) ->
  << Request:Length/binary, Rest/binary >> = Buffer,
  case parse_body(Request) of
    {ok, AJP_Request} -> {ok, AJP_Request, Rest};
    {ok, unknown_request, Request} -> {ok, unknown_request, Buffer}
  end.
  
parse_body( << 2, AJP_ForwardRequest/binary >> ) ->
  parse_forward_request(AJP_ForwardRequest);
parse_body( << 7, _/binary >> ) ->
  {ok, shutdown_request};
parse_body( << 10, _/binary >> ) ->
  {ok, cping_request};  
parse_body( << 18, 52, Length:16, AJP_ForwardRequest:Length/binary >> ) ->
  parse_body(AJP_ForwardRequest);
parse_body(Req) ->
  {ok, unknown_request, Req}.
  
parse_forward_request(<< 1, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "OPTIONS"});
parse_forward_request(<< 2, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "GET"});
parse_forward_request(<< 3, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "HEAD"});
parse_forward_request(<< 4, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "POST"});
parse_forward_request(<< 5, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "PUT"});
parse_forward_request(<< 6, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "DELETE"});
parse_forward_request(<< 7, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "TRACE"});
parse_forward_request(<< 8, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "PROPFIND"});
parse_forward_request(<< 9, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "PROPPATCH"});
parse_forward_request(<< 10, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "MKCOL"});
parse_forward_request(<< 11, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "COPY"});
parse_forward_request(<< 12, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "MOVE"});
parse_forward_request(<< 13, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "LOCK"});
parse_forward_request(<< 14, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "UNLOCK"});
parse_forward_request(<< 15, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "ACL"});
parse_forward_request(<< 16, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "REPORT"});
parse_forward_request(<< 17, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "VERSION-CONTROL"});
parse_forward_request(<< 18, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "CHECKIN"});
parse_forward_request(<< 19, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "CHECKOUT"});
parse_forward_request(<< 20, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "UNCHECKOUT"});
parse_forward_request(<< 21, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "SEARCH"});
parse_forward_request(<< 22, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "MKWORKSPACE"});
parse_forward_request(<< 23, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "UPDATE"});
parse_forward_request(<< 24, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "LABEL"});
parse_forward_request(<< 25, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "MERGE"});
parse_forward_request(<< 26, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "BASELINE_CONTROL"});
parse_forward_request(<< 27, AJP_Request/binary >>) ->
  parse_request_body(AJP_Request, #ajp_request_envelope{method = "MKACTIVITY"});
parse_forward_request(Req) ->
  {error, "Unknown request method", Req}.
  
parse_request_body(AJP_Request, AJP_Request_Envelope) ->
  {Protocol, A1} = parse_string(AJP_Request),
  {RequestURI, A2} = parse_string(A1),
  {RemoteAddr, A3} = parse_string(A2),
  {RemoteHost, A4} = parse_string(A3),
  {ServerName, A5} = parse_string(A4),
  <<Port:16, IsSSL:8, HeaderLength:16, A6/binary>> = A5,
  {ok, AJP_Headers, Rest} = decode_encoded_headers(A6, [], HeaderLength),
  {ok, AJP_Attributes} = decode_request_attributes(Rest,[]),
  
  {ok, AJP_Request_Envelope#ajp_request_envelope{
    protocol = Protocol,
    request_uri = RequestURI, 
    remote_address = RemoteAddr, 
    remote_host = RemoteHost, 
    server_name = ServerName, 
    port = Port, 
    is_ssl = IsSSL, 
    headers = AJP_Headers,
    attributes = AJP_Attributes
  }}.

decode_encoded_headers(Rest, HeaderAcc, HeaderLength) when HeaderLength =< 0 ->
  {ok, HeaderAcc, Rest};
decode_encoded_headers(<<AQ, AQ1, Size:16, Name:Size/binary, 0, Rest/binary>>, HeaderAcc, HeaderLength) when AQ == 16#A0 ->
  decode_encoded_headers(Rest, [{lookup_header_name(AQ1), binary_to_list(Name)} | HeaderAcc], (HeaderLength - 1));
decode_encoded_headers(<<HeaderSize:16, HeaderName:HeaderSize/binary, 0,
                    HeaderVSize:16, HeaderV:HeaderVSize/binary,0,
                    Rest/binary>>, HeaderAcc, HeaderLength) when HeaderSize =< 16#9999 ->
  decode_encoded_headers(Rest, [{binary_to_list(HeaderName), binary_to_list(HeaderV)} | HeaderAcc], (HeaderLength - 1));
decode_encoded_headers(_Rest, _HeaderAcc, HeaderLength) ->
  {error, "insufficient headers in request", HeaderLength}.

decode_request_attributes(<< 16#FF:8 >>, Attributes) ->
  {ok, Attributes};
decode_request_attributes(<< 16#0A, ASize:16, AName:ASize/binary, 0, 
                          VSize:16, Value:VSize/binary, 0, Rest/binary >>, Attributes) ->
  decode_request_attributes(Rest, [{AName, Value} | Attributes]);
decode_request_attributes(<< AttrName:8, Size:16, Value:Size/binary, 0, Rest/binary >>, Attributes) ->
  decode_request_attributes(Rest, [{lookup_attribute_name(AttrName), Value} | Attributes]).
  
lookup_header_name(16#01) ->
  "accept";
lookup_header_name(16#02) ->
  "accept-charset";
lookup_header_name(16#03) ->
  "accept-encoding";
lookup_header_name(16#04) ->
  "accept-language";
lookup_header_name(16#05) ->
  "authorization";
lookup_header_name(16#06) ->
  "connection";
lookup_header_name(16#07) ->
  "content-type";
lookup_header_name(16#08) ->
  "content-length";
lookup_header_name(16#09) ->
  "cookie";
lookup_header_name(16#0A) ->
  "cookie2";
lookup_header_name(16#0B) ->
  "host";
lookup_header_name(16#0C) ->
  "pragma";
lookup_header_name(16#0D) ->
  "referer";
lookup_header_name(16#0E) ->
  "user-agent".

% Attribute lookup
lookup_attribute_name(16#01) ->
  "context";
lookup_attribute_name(16#02) ->
  "servlet_path";
lookup_attribute_name(16#03) ->
  "remote_user";
lookup_attribute_name(16#04) ->
  "auth_type";
lookup_attribute_name(16#05) ->
  "query_string";
lookup_attribute_name(16#06) ->
  "route";
lookup_attribute_name(16#07) ->
  "ssl_cert";
lookup_attribute_name(16#08) ->
  "ssl_cipher";
lookup_attribute_name(16#09) ->
  "ssl_session";
lookup_attribute_name(16#0A) ->
  "req_attribute";
lookup_attribute_name(16#0B) ->
  "ssl_key_size";
lookup_attribute_name(16#0C) ->
  "secret";
lookup_attribute_name(16#0D) ->
  "stored_method".
  
  
parse_string(<< Length:16, AJPString:Length/binary, 0, RestOfBody/binary >>) when Length < 16#FFFF ->
  {binary_to_list(AJPString), RestOfBody};
parse_string(<< 16#FFFF:16, Body/binary >>) ->
  {[], Body}.

%% Resonse related 
encode_body_response(Binary, Length) ->
  << $A, $B, (Length + 4):16, 3:8, Length:16, Binary/binary, 0:8 >>.

encode_header_response(ResponseHeaders) ->
  {ok, NumHeaders, ResponseBytes} = encode_headers(ResponseHeaders#ajp_response_envelope.headers, 0, <<>>),
  Status = ResponseHeaders#ajp_response_envelope.status,
  Message = encode_string(ResponseHeaders#ajp_response_envelope.message),
  MessageSize = byte_size(Message),
  ResponseSize = byte_size(ResponseBytes),
  << $A, $B, (5 + ResponseSize + MessageSize):16, 4:8, Status:16, Message/binary, NumHeaders:16, ResponseBytes/binary >>.

encode_end_response() ->
  << $A, $B, 2:16, 5:8, 0:8 >>.
  
encode_get_body_response(Length) ->
  << $A, $B, 3:16, 6:8, Length:16 >>.
  
encode_headers([], HeadersLength, Bytes) ->
  {ok, HeadersLength, Bytes};
encode_headers([{Name, Val} | Headers], HeadersLength, Bytes) ->
  EName = encode_header_name(Name),
  EVal = encode_string(Val),
  encode_headers(Headers, HeadersLength + 1, << Bytes/binary, EName/binary, EVal/binary >>).
  
encode_header_name("content-type") ->
  << 16#A001:16 >>;
encode_header_name("content-language") ->
  << 16#A002:16 >>;
encode_header_name("content-length") ->
  << 16#A003:16 >>;
encode_header_name("date") ->
  << 16#A004:16 >>;
encode_header_name("last-modified") ->
  << 16#A005:16 >>;
encode_header_name("location") ->
  << 16#A006:16 >>;
encode_header_name("set-cookie") ->
  << 16#A007:16 >>;
encode_header_name("set-cookie2") ->
  << 16#A008:16 >>;
encode_header_name("servlet-engine") ->
  << 16#A009:16 >>;
encode_header_name("status") ->
  << 16#A00A:16 >>;
encode_header_name("www-authenticate") ->
  << 16#A00B:16 >>;
encode_header_name(Header) ->
  L = length(Header),
  B = list_to_binary(Header),
  << L:16, B/binary, 0 >>.
  
encode_string(String) ->
  L = length(String),
  Bytes = list_to_binary(String),
  << L:16, Bytes/binary, 0:8 >>.
  
