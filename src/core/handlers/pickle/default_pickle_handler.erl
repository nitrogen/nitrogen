% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_pickle_handler).
-behaviour (default_pickle_handler).
-include ("wf.inc").
-define (SIGNKEY, "Gabagaba").
-export ([
	init/1, 
	finish/2,
	pickle/3,
	depickle/3
]).

init(Context) -> 
	{ok, Context, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

pickle(Data, Context, State) ->
	B = term_to_binary(Data, [compressed]),
	<<Signature:4/binary, _/binary>> = erlang:md5([B, ?SIGNKEY]),
	PickledData = modified_base64_encode(<<Signature/binary, B/binary>>),
	{ok, PickledData, Context, State}.
	
depickle(PickledData, Context, State) ->
	try
		<<S:4/binary, B/binary>> = modified_base64_decode(wff:to_binary(PickledData)),
		<<S:4/binary, _/binary>> = erlang:md5([B, ?SIGNKEY]),
		Data = binary_to_term(B),
		{ok, Data, Context, State}
	catch _Type : _Message ->
		{ok, undefined, Context, State}
	end.

%%% PRIVATE FUNCTIONS
	
% modified_base64_encode/1 
%	- Replace '+' and '/' with '-' and '_', respectively. 
% - Strip '='.
modified_base64_encode(B) -> m_b64_e(base64:encode(B), <<>>).
m_b64_e(<<>>, Acc) -> Acc;
m_b64_e(<<$+, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $->>);
m_b64_e(<<$/, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $_>>);
m_b64_e(<<$=, Rest/binary>>, Acc) -> m_b64_e(Rest, Acc);
m_b64_e(<<H,  Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, H>>).
		
% modified_base64_decode/1 
% - Replace '-' and '_' with '+' and '/', respectively. 
% - Pad with '=' to a multiple of 4 chars.
modified_base64_decode(B) -> base64:decode(m_b64_d(B, <<>>)).
m_b64_d(<<>>, Acc) when size(Acc) rem 4 == 0 -> Acc;
m_b64_d(<<>>, Acc) when size(Acc) rem 4 /= 0 -> m_b64_d(<<>>, <<Acc/binary, $=>>);
m_b64_d(<<$-, Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, $+>>);
m_b64_d(<<$_, Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, $/>>);
m_b64_d(<<H,  Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, H>>).
