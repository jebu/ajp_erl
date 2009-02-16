%%%-------------------------------------------------------------------
%%% File:      ajp_records.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2008 Jebu Ittiachen
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-13 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-author('jebui@yahoo-inc.com').

%% ajp request structure
-record(ajp_request_envelope, {method, protocol, request_uri, remote_address, remote_host, server_name, port, is_ssl, headers = [], attributes = []}).
-record(ajp_response_envelope, {status = 200, message = "OK", headers = [{"content-length","0"},{"servlet-engine","AJPERL"}]}).
