% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.
%
% Large portions of this file are from mochiweb_multipart.erl
% Copyright 2007 Mochi Media, Inc., written by Bob Ippolito <bob@mochimedia.com>.
% 
% Bob, thanks for crafting an awesome web server. Hopefully you are
% okay with Nitrogen borrowing some of the code. If not, let me (Rusty) know,
% and I'll yank the code out.

-module(wf_multipart).
-include ("wf.inc").
-export ([
	parse_upload/0
]).

-define(CHUNKSIZE, 4096).
-define(IDLE_TIMEOUT, 30000).
-record(mp, {state, boundary, length, buffer, callback}).
-record(state, {queryargs = [], filename=undefined, localfiledata=undefined}).

parse_upload() -> 
	ScratchDir = nitrogen:get_scratch_directory(),
	file:make_dir(ScratchDir),
	TempFileName = wf:to_list(wf_utils:pickle(erlang:now())),
	LocalFileData = filename:join(ScratchDir, TempFileName),
 	State = #state { localfiledata=LocalFileData },
	Callback = fun(X) -> callback(X, undefined, State) end,
	parse(Callback).

callback({headers, Headers}, _, State) ->
	get_name_and_filename(Headers),
	case get_name_and_filename(Headers) of
		{FormKey, FileName} when FileName == undefined; FileName == "" -> 
			fun(X) -> callback(X, FormKey, State) end;
		{_FormKey, FileName} ->			
			State1 = State#state { filename=FileName },
			fun(X) -> callback(X, file, State1) end
	end;

callback({body, _}, undefined, _) ->	
	throw(unexpected_data_received);

callback({body, Data}, file, State = #state { localfiledata=LocalFileData }) ->	
	{ok, File} = file:open(LocalFileData, [raw, append]),
	file:write(File, Data),
	file:close(File),
	fun(X) -> callback(X, file, State) end;

callback({body, Data}, FormKey, State) ->	
	QueryArgs1 = [{FormKey, Data}|State#state.queryargs],
	State1 = State#state { queryargs=QueryArgs1 },
	fun(X) -> callback(X, undefined, State1) end;

callback(body_end, _, State) ->	
	fun(X) -> callback(X, undefined, State) end;

callback(eof, _, State) ->
	{ok, State#state.queryargs, State#state.filename, State#state.localfiledata};
	
callback(Directive, _, _) ->
	throw({invalid_callback, Directive}).
	
get_name_and_filename(Headers) ->
	F = fun(Props) -> [{string:to_lower(Key), Value} || {Key, Value} <- Props] end,
	{_, FormData} = proplists:get_value("content-disposition", F(Headers)),
	Name = proplists:get_value("name", F(FormData)),
	FileName = proplists:get_value("filename", F(FormData)),
	{Name, FileName}.

parse(Callback) ->
	% Figure out the boundary and length...
	Length = list_to_integer(wf_platform:get_header(content_length)),
	Boundary = iolist_to_binary(get_boundary(wf_platform:get_header(content_type))),
	Prefix = <<"\r\n--", Boundary/binary>>,
	
	% Get whatever the yaws/mochiweb/inets has already read...
	Chunk = wf_platform:get_request_body(),
	HasChunk = 
		Chunk /= undefined andalso 
		((is_binary(Chunk) andalso size(Chunk) > 0) orelse (is_list(Chunk) andalso length(Chunk) > 0)),
	Chunk1 = case HasChunk of
		true  -> 
			wf:to_binary(Chunk);
		false -> 
			read_chunk(Length)
	end,
	
	% Parse the first chunk...
	Length1 = Length - size(Chunk1),
	BS = size(Boundary),
	<<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk1,
	
	% Parse the headers...
	MP = #mp{boundary=Prefix, length=Length1, buffer=Rest, callback=Callback},
	feed_mp(headers, MP).

feed_mp(headers, State=#mp{buffer=Buffer, callback=Callback}) ->
	% Find the end of the headers...
	FindResult = find_in_binary(<<"\r\n\r\n">>, Buffer),
	{State1, P} = case FindResult of
		{exact, N} -> 
			{State, N};
		_ ->
		  S1 = read_more(State),
		  %% Assume headers must be less than ?CHUNKSIZE
		  {exact, N} = find_in_binary(<<"\r\n\r\n">>, S1#mp.buffer),
		  {S1, N}
	end,
	
	% Split out the headers...
	<<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
	
	% Call back with headers...
	NextCallback = Callback({headers, parse_headers(Headers)}),
	
	% Parse the body...
	feed_mp(body, State1#mp{buffer=Rest, callback=NextCallback});
	
feed_mp(body, State=#mp{boundary=Prefix, buffer=Buffer, callback=Callback}) ->
	FindResult = find_boundary(Prefix, Buffer),
	case FindResult of
		{end_boundary, Size, Skip} ->
			% Feed data into the callback function...
			<<Data:Size/binary, _:Skip/binary, _Rest/binary>> = Buffer,
			C1 = Callback({body, Data}),
			
			% Finish...
			C2 = C1(body_end),
			C2(eof);
		
		{next_boundary, Size, Skip} ->
			% Feed data into the callback function...
			<<Data:Size/binary, _:Skip/binary, Rest/binary>> = Buffer,
			C1 = Callback({body, Data}),
			
			% Parse the next set of headers...
			State1 = State#mp{callback=C1(body_end), buffer=Rest},
			feed_mp(headers, State1);
			
		{maybe, Size} ->
			% Feed the data we have into the callback function...
			<<Data:Size/binary, Rest/binary>> = Buffer,
			C1 = Callback({body, Data}),
			
			% Continue parsing the body...
			State1 = State#mp{callback=C1, buffer=Rest},
			feed_mp(body, read_more(State1));
			
		not_found ->
			% No boundary found, so this is pure data. Feed
			% it into the callback function...
			Data = Buffer,
			C1 = Callback({body, Data}),
			
			% Continue parsing the body...
			State1 = State#mp{callback=C1, buffer = <<>>},
			feed_mp(body, read_more(State1))
	end.











% parse_form(Req, FileHandler) ->
% 	Callback = fun (Next) -> parse_form_outer(Next, FileHandler, []) end,
% 	{_, _, Res} = parse_multipart_request(Req, Callback),
% 	Res.
% 
% parse_form_outer(eof, _, Acc) ->
% 	lists:reverse(Acc);
% parse_form_outer({headers, H}, FileHandler, State) ->
% 	{"form-data", H1} = proplists:get_value("content-disposition", H),
% 	Name = proplists:get_value("name", H1),
% 	Filename = proplists:get_value("filename", H1),
% 	case Filename of
% 		undefined ->
% 			fun (Next) ->
% 					parse_form_value(Next, {Name, []}, FileHandler, State)
% 			end;
% 		_ ->
% 			ContentType = proplists:get_value("content-type", H),
% 			Handler = FileHandler(Filename, ContentType),
% 			fun (Next) ->
% 					parse_form_file(Next, {Name, Handler}, FileHandler, State)
% 			end
% 	end.
% 
% parse_form_value(body_end, {Name, Acc}, FileHandler, State) ->
% 	Value = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
% 	State1 = [{Name, Value} | State],
% 	fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
% parse_form_value({body, Data}, {Name, Acc}, FileHandler, State) ->
% 	Acc1 = [Data | Acc],
% 	fun (Next) -> parse_form_value(Next, {Name, Acc1}, FileHandler, State) end.
% 
% parse_form_file(body_end, {Name, Handler}, FileHandler, State) ->
% 	Value = Handler(eof),
% 	State1 = [{Name, Value} | State],
% 	fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
% parse_form_file({body, Data}, {Name, Handler}, FileHandler, State) ->
% 	H1 = Handler(Data),
% 	fun (Next) -> parse_form_file(Next, {Name, H1}, FileHandler, State) end.
% 
% default_file_handler(Filename, ContentType) ->
% 	default_file_handler_1(Filename, ContentType, []).
% 
% default_file_handler_1(Filename, ContentType, Acc) ->
% 	fun(eof) ->
% 			Value = iolist_to_binary(lists:reverse(Acc)),
% 			{Filename, ContentType, Value};
% 	   (Next) ->
% 			default_file_handler_1(Filename, ContentType, [Next | Acc])
% 	end.


%% parse_headers/0 - 
%% Given a binary, return a proplist of headers.
parse_headers(<<>>) -> 
	[];
parse_headers(Binary) ->
	parse_headers(Binary, []).

parse_headers(Binary, Acc) ->
	case find_in_binary(<<"\r\n">>, Binary) of
		{exact, N} ->
			<<Line:N/binary, "\r\n", Rest/binary>> = Binary,
			parse_headers(Rest, [split_header(Line) | Acc]);
		not_found ->
			lists:reverse([split_header(Binary) | Acc])
	end.

split_header(Line) ->
	{Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end, binary_to_list(Line)),
	{string:to_lower(string:strip(Name)),
	parse_header(Value)}.
	
%% read_chunk/2 -
%% Read a new chunk of data from the supplied socket.

read_chunk(Length) when Length > 0 ->
	case Length < ?CHUNKSIZE of
		true  -> wf_platform:recv_from_socket(Length, ?IDLE_TIMEOUT);
		false -> wf_platform:recv_from_socket(?CHUNKSIZE, ?IDLE_TIMEOUT)
	end.


%% read_more/1 -
%% Read more data from the socket, return a new multipart state.
read_more(State=#mp{length=Length, buffer=Buffer}) ->
	Data = read_chunk(Length),
	Buffer1 = <<Buffer/binary, Data/binary>>,
	State#mp{length=Length - size(Data), buffer=Buffer1}.



%% find_in_binary/2 -
%% Find the location of a binary value
%% within a longer binary value.
find_in_binary(B, Data) when size(B) > 0 ->
	case size(Data) - size(B) of
		Last when Last < 0 ->
			partial_find(B, Data, 0, size(Data));
		Last ->
			find_in_binary(B, size(B), Data, 0, Last)
	end.

find_in_binary(B, BS, D, N, Last) when N =< Last->
	case D of
		<<_:N/binary, B:BS/binary, _/binary>> ->
			{exact, N};
		_ ->
			find_in_binary(B, BS, D, 1 + N, Last)
	end;
	
find_in_binary(B, BS, D, N, Last) when N =:= 1 + Last ->
	partial_find(B, D, N, BS - 1).

partial_find(_B, _D, _N, 0) ->
	not_found;
partial_find(B, D, N, K) ->
	<<B1:K/binary, _/binary>> = B,
	case D of
		<<_Skip:N/binary, B1:K/binary>> ->
			{partial, N, K};
		_ ->
			partial_find(B, D, 1 + N, K - 1)
	end.


%% find_boundary/2 - 
%% Given a binary boundary prefix, and binary data,
%% return:
%%
%% - {next_boundary, LengthOfData, OffsetToNextData} if the boundary was found.
%% - {end_boundary, LengthOfData, OffsetToNextData>} if the ending of a multipart section was found.
%% - {maybe, LengthOfData} if there is not enough data present.
%% - not_found if the boundary was not found.
%%
%% Where:
%%
%% - LengthOfData is the longth of the current block of data.
%%
%% - OffsetToNextData is the additional bytes, after the 
%%   current block of data, until the next block starts.

find_boundary(Prefix, Data) ->
	case find_in_binary(Prefix, Data) of
		{exact, Skip} ->
			PrefixSkip = Skip + size(Prefix),
			case Data of
				<<_:PrefixSkip/binary, "\r\n", _/binary>> ->
					{next_boundary, Skip, size(Prefix) + 2};
				<<_:PrefixSkip/binary, "--\r\n", _/binary>> ->
					{end_boundary, Skip, size(Prefix) + 4};
				_ when size(Data) < PrefixSkip + 4 ->
					%% Underflow
					{maybe, Skip};
				_ ->
					%% False positive
					not_found
			end;
		{partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
			%% Underflow
			{maybe, Skip};
		_ ->
			not_found
	end.



%%% BOUNDARY %%%

get_boundary(ContentType) ->
	{"multipart/form-data", Opts} = parse_header(ContentType),
	case proplists:get_value("boundary", Opts) of
		S when is_list(S) -> S
	end.
	
%% @spec parse_header(string()) -> {Type, [{K, V}]}
%% @doc  Parse a Content-Type like header, return the main Content-Type
%%	   and a property list of options.
parse_header(String) ->
	%% TODO: This is exactly as broken as Python's cgi module.
	%%	   Should parse properly like mochiweb_cookies.
	[Type | Parts] = [string:strip(S) || S <- string:tokens(String, ";")],
	F = fun (S, Acc) ->
				case lists:splitwith(fun (C) -> C =/= $= end, S) of
					{"", _} ->
						%% Skip anything with no name
						Acc;
					{_, ""} ->
						%% Skip anything with no value
						Acc;
					{Name, [$\= | Value]} ->
						[{string:to_lower(string:strip(Name)),
						  unquote_header(string:strip(Value))} | Acc]
				end
		end,
	{string:to_lower(Type),
	 lists:foldr(F, [], Parts)}.

unquote_header("\"" ++ Rest) ->
	unquote_header(Rest, []);
unquote_header(S) ->
	S.
unquote_header("", Acc) ->
	lists:reverse(Acc);
unquote_header("\"", Acc) ->
	lists:reverse(Acc);
unquote_header([$\\, C | Rest], Acc) ->
	unquote_header(Rest, [C | Acc]);
unquote_header([C | Rest], Acc) ->
	unquote_header(Rest, [C | Acc]).