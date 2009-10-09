% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_pickle).
-include ("wf.inc").
-define (SIGNKEY, "Gabagaba").
-export ([
	pickle/1, pickle/2,
	depickle/1, depickle/2
]).

% Does a plain old term_to_binary...
pickle(Data) ->
	B = term_to_binary({Data, now()}, [compressed]),
	<<Signature:4/binary, _/binary>> = erlang:md5([B, ?SIGNKEY]),
	_PickledData = modified_base64_encode(<<Signature/binary, B/binary>>).
	
depickle(undefined) -> undefined;
depickle(PickledData) ->
	try
		<<S:4/binary, B/binary>> = modified_base64_decode(wf:to_binary(PickledData)),
		<<S:4/binary, _/binary>> = erlang:md5([B, ?SIGNKEY]),
		{Data, _PickleTime} = binary_to_term(B),
		Data
	catch _Type : _Message ->
		undefined
	end.
	
%%% PICKLE WITH SCHEMA %%%

pickle(Schema, Data) ->
	% Use BinaryVice to make the data smaller...
	B = vice:to_binary({Schema, {integer@, integer@, integer@}}, {Data, now()}),
	<<Signature:4/binary, _/binary>> = erlang:md5([B, ?SIGNKEY]),
	_PickledData = modified_base64_encode(<<Signature/binary, B/binary>>).

depickle(Schema, PickledData) ->
	try
		<<S:4/binary, B/binary>> = modified_base64_decode(wf:to_binary(PickledData)),
		<<S:4/binary, _/binary>> = erlang:md5([B, ?SIGNKEY]),
		{Data, _PickleTime} = vice:from_binary({Schema, {integer@, integer@, integer@}}, B),
		Data
	catch _Type : _Message ->
		undefined
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
