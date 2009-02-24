%%%-------------------------------------------------------------------
%%% File:      test_ajp_mount.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2009 Jebu Ittiachen
%%%
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
%%%
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-16 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-module(test_ajp_mount).
-author('jebui@yahoo-inc.com').
-behaviour(gen_ajp_handler).

%% API
-export([handle_request/2, handle_get_request/2, handle_post_request/2, handle_put_request/2, handle_delete_request/2]).

-include_lib("../include/ajp_records.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------
handle_request(Message, PPid) when is_record(Message, ajp_request_envelope) ->
  ok = gen_ajp_handler:send_headers(#ajp_response_envelope{headers=[{"content-length", "12"}]}, PPid),
  ok = gen_ajp_handler:send_data(<<"testing data">>, PPid),
  gen_ajp_handler:end_request(PPid).

%
handle_get_request(_Message, PPid) ->
  ok = gen_ajp_handler:send_headers(#ajp_response_envelope{headers=[{"content-length", "12"}]}, PPid),
  ok = gen_ajp_handler:send_data(<<"testing data">>, PPid),
  gen_ajp_handler:end_request(PPid).

%
handle_post_request(_Message, PPid) ->
  {ok, Data, AL} = gen_ajp_handler:request_data(12, PPid),
  ok = gen_ajp_handler:send_headers(#ajp_response_envelope{headers=[{"content-length", integer_to_list(AL)}]}, PPid),
  ok = gen_ajp_handler:send_data(Data, PPid),
  gen_ajp_handler:end_request(PPid).

%
handle_put_request(_Message, PPid) ->
  ok = gen_ajp_handler:send_headers(#ajp_response_envelope{headers=[{"content-length", "12"}]}, PPid),
  ok = gen_ajp_handler:send_data(<<"testing data">>, PPid),
  gen_ajp_handler:end_request(PPid).

%
handle_delete_request(_Message, PPid) ->
  ok = gen_ajp_handler:send_headers(#ajp_response_envelope{headers=[{"content-length", "12"}]}, PPid),
  ok = gen_ajp_handler:send_data(<<"testing data">>, PPid),
  gen_ajp_handler:end_request(PPid).
  
%%====================================================================
%% Internal functions
%%====================================================================

