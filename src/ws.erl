-module(ws).
-compile(export_all).
-import(mochijson2, [encode/1, decode/1]).

-export([start_link/1,
	 init/3,      websocket_init/3,
	 handle/2,    websocket_handle/3, 
	 terminate/3, websocket_terminate/3,
	 websocket_info/3,
	 append_div/3,
	 pre/1,
	 fill_div/3
	]).

%% env has only one parameter - reserved for future expansion

-record(env, {root}).

start([Config]) ->
    File = (atom_to_list(Config)),
    io:format("ws start:~p~n",[File]),
    Conf = read_config(File),
    start_link(Conf).

require(Key, Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value,{_,Val}} ->
	    Val;
	false ->
	    io:format("** fatal error no paramamer called:~p~n",[Key]),
	    init:stop()
	end.

read_config(File) ->
    Abs = File ++ ".config",
    io:format("Consulting:~p~n",[Abs]),
    case file:consult(Abs) of
	{ok, Terms} ->
	    io:format("Starting with:~p~n",[Terms]),
	    Terms;
	{error, Why} ->
	    io:format("Error consulting:~p~n",[Abs])
    end.

start_link(Conf) ->
    ok = application:start(crypto),
    ok = application:start(ranch),  
    ok = application:start(cowboy),
    ok = web_server_start(Conf),
    receive
	after 
	    infinity ->
		true
	end.
    
web_server_start(Conf) ->
    Port = require(port, Conf), 
    E0 = Conf,
    Dispatch = cowboy_router:compile([{'_',[{'_', ?MODULE, Conf}]}]),  
    
    %% server is the name of this module
    NumberOfAcceptors = 10,
    Status = 
	cowboy:start_http(my_named_thing,
			  NumberOfAcceptors,
			  [{port, Port}],
			  [{env,[{dispatch, Dispatch}]}]),
    case Status of
	{error, _} ->
	    io:format("websockets could not be started -- "
		      "port ~p probably in use~n", [Port]),
	    init:stop();
	{ok, _Pid} ->
	    io:format("webserver with websockets started on port:~p~n",[Port])
    end.

init(Conf, Req, E0) ->
    io:format("Init: Conf:~p~n",[E0]),
    Resource = path(Req),
    %% io:format("init Resource =~p Env=~p~n",[Resource, E0]),
    case Resource of
	["/", "websocket",_] ->
	    %% The upgrade return value will cause cowboy
	    %% to call this module at the entry point
	    %% websocket_init
	    io:format("upgrading:~n"),
	    {upgrade, protocol, cowboy_websocket};
	_ ->
	    {ok, Req, E0}
    end.

handle(Req, Env) ->
    io:format("~s:Calling handle1 path=~p ~n",[?MODULE, path(Req)]),
    handle1(path(Req), Req, Env).

unhex("%22" ++ T) -> [$"|unhex(T)];
unhex("%20" ++ T) -> [$\s|unhex(T)];
unhex([H|T])      -> [H|unhex(T)];
unhex([])         -> []. 

handle1(["/","eval"], Req, Env) ->
    {ok, Q, Req1} = cowboy_req:body(Req),
    Json = (catch decode(Q)),
    io:format("Q=~p~n",[Json]),
    {struct,[{<<"mod">>,MS},{<<"func">>,FS},{<<"args">>,Args}]} = Json,
    Mod = b2a(MS),
    Func = b2a(FS),
    Ret = apply1(Mod, Func, Args),
    reply_json(Ret, Req, Env);
handle1(["/", "write_json_file"], Req, Env) ->
    [{<<"filename">>,FB},{<<"data">>, Bin}] = args(Req),
    Q1 = unhex(binary_to_list(Bin)),
    io:format("Q1:~p~n",[Q1]),
    Json = (catch decode(Q1)),
    Filename = "./website/" ++ binary_to_list(FB),
    io:format("Writing:~p~n",[Filename]),
    {ok, S} =  file:open(Filename, [write]),
    io:format(S,"~p.~n",[Json]),
    file:close(S),
    reply_html("yes", Req, Env);
handle1(["/", "read_json_file"], Req, Env) ->
    [{<<"filename">>,FB}] = args(Req),
    Filename = binary_to_list(FB),
    {ok, [X]} = file:consult(Filename),
    B = encode(X),
    reply_html(list_to_binary(B), Req, Env);
handle1(["/", "write_file"], Req, Env) ->
    io:format("debiug:~p~n",[args(Req)]),
    [{<<"file">>, F0}] = args(Req),
    F1 = binary_to_list(F0),
    {ok, Bin, Req1} = cowboy_req:body(Req),
    F2 = os:getenv("HOME") ++ "/Dropbox/chandler/" ++ F1,
    ok = file:write_file(F2, Bin),
    io:format("wrote:~s ~p bytes~n",[F2, size(Bin)]),
    reply_html("ok", Req1, Env);
handle1(["/", "read_file"], Req, Env) ->
   [{<<"file">>, F0}] = args(Req),
    F1 = binary_to_list(F0),
    F2 = os:getenv("HOME") ++ "/Dropbox/chandler/" ++ F1,
    io:format("read: ~s~n",[F2]),
    {ok, B} = file:read_file(F2),
    io:format("read: ~s ~p bytes~n",[F2, size(B)]),
    reply_html(B, Req, Env);

handle1(["/", "store_data"], Req, Env) ->
    %% io:format("extracting args:~p~n",[Req]),
    {ok, Q, Req1} = cowboy_req:body(Req),
    io:format("save ~p bytes~n",[size(Q)]),
    %% Q1 = unhex(binary_to_list(Q)),
    %% io:format("Q1:~p~n",[Q1]),
    write_latest(Q),
    %% debug
    Args = (catch decode(Q)),
    write_debug(Args),
    reply_html("ok", Req1, Env);
handle1(_, Req, Env) ->
    File1 = file_path(Req),
    File2 = map(File1, Env),
    io:format("here12334:~p (isdir=~p)~n",[File2,filelib:is_dir(File2)]),
    case filelib:is_dir(File2) of
	true ->
	    list_dir(File2, Req, Env);
	false ->
	    case filename:extension(File2) of
		[] ->
		    serve_file(File2, Req, Env);
		Ext ->
		    case known_file_type(Ext) of
			true ->
			    serve_file(File2, Req, Env);
			false ->
			    serve_wierdo_file(Ext, File2, Req, Env)
		    end
	    end
    end.

map(File, Conf) ->
    io:format("map: ~p with ~p~n", [File, Conf]),
    map1(File, Conf).

map1(File, [{map,Stem,O}|T]) ->
    case remove_prefix(Stem, File) of
	{yes, Rest} ->
	    F1 = O ++ Rest,
	    io:format("Rest:~p expand:~p~n",[Rest, F1]),
	    expand(F1);
	no ->
	    map1(File, T)
    end;
map1(File, [_|T]) ->
    map1(File, T);
map1(File, []) ->
    File.

remove_prefix([], T)         -> {yes, T};
remove_prefix([H|T1],[H|T2]) -> remove_prefix(T1, T2);
remove_prefix(_, _)          -> no. 

expand("${HOME}" ++ T) -> os:getenv("HOME") ++ expand(T);
expand([H|T])          -> [H|expand(T)];
expand([])             -> [].

serve_wierdo_file("." ++ Str, Res, Req, Env) ->
    Mod = list_to_atom("wierdo_handler_" ++ Str),
    Args = args(Req),
    io:format("erl3_webserver Calling Mod:~p Res:~p Args:~p~n",[Mod,Res,Args]),
    Html = Mod:file2html(Res, Args),
    reply_html(Html, Req, Env).

beard_expand(A, Req, Env) ->
    Html = beard:file2html(A),
    reply_html(Html, Req, Env).

ehe_expand(A, Req, Env) ->
    Html = ehe2:file2html(A),
    reply_html(Html, Req, Env).

serve_file(File, Req, Env) ->
    io:format("serve_abs:~p~n",[File]),
    Val = file:read_file(File),
    case Val of 
	{error, _} ->
	    io:format("*** no page called ~p~n",[File]),
	    reply_html(pre({no_page_called,File}), Req, Env);
	{ok, Bin} ->
	    Ext = filename:extension(File),
	    Bin1 = add_wrapper(Ext, Bin),
	    {ok, Req1} = send_page(classify_extension(Ext), Bin1, Req),
	    {ok, Req1, Env}
    end.

add_wrapper(".erl", B) ->
    ["<pre>", B, "</pre>"];
add_wrapper(_, B) ->
    B.


list_dir(Root, Req, Env) ->
    io:format("List dir:~p~n",[Root]),
    {ok, Files} = file:list_dir(Root),
    Files1 = [add_slash(I, Root) || I <- Files],
    L1 = [["<li><a href='",I,"'>",I,"</a></li>\n"] || I <- lists:sort(Files1)],
    reply_html(["<h1> Directory ",Root, "</h1>\n",
		"<ul>\n",L1,"</ul>\n"], Req, Env).

add_slash(I, Root) ->
    %% io:format("Add slash:~p ~p~n",[I,Root]),
    Full = filename:join(Root, I),
    case filelib:is_dir(Full) of
	true ->
	    I ++ "/";
	false ->
	    I
    end.

send_page(Type, Data, Req) ->
    cowboy_req:reply(200, [{<<"Content-Type">>,
			    list_to_binary(mime_type(Type))}],
		     Data, Req).

classify_extension(".gif") -> gif;
classify_extension(".jpg") -> jpg;
classify_extension(".png") -> png;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(".mp3") -> mp3;
classify_extension(".wav") -> wav;
classify_extension(".swf") -> swf;
classify_extension(_)      -> html.

known_file_type(X) ->
    lists:member(X, [".swf",
		     ".wav", ".mp3", ".ico",".html", 
		     ".js", ".jpg", ".css", ".png", ".gif"]).


mime_type(ico)     -> "image/x-icon";
mime_type(gif)     -> "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
mime_type(mp3)     -> "audio/mpeg";
mime_type(wav)     -> "audio/wav";
mime_type(special) -> "text/plain; charset=x-user-defined";
mime_type(json)    -> "application/json";
mime_type(swf)     -> "application/x-shockwave-flash";
mime_type(html)    -> "text/html";
mime_type(xul)     -> "application/vnd.mozilla.xul+xml";
mime_type(js)      -> "application/x-javascript";
mime_type(svg)     -> "image/svg+xml".


pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

path(Req) ->
    {Path,_} = cowboy_req:path(Req),
    P = filename:split(binary_to_list(Path)),
    io:format("Path=~p~n",[P]),
    P.

file_path(Req) ->
    {Path,_} = cowboy_req:path(Req),
    binary_to_list(Path).

args(Req) ->
    {Args, _} = cowboy_req:qs_vals(Req),
    Args.


form_data(Req) ->
    {ok, Data, _} = cowboy_req:body_qs(Req),
    Data.




%% reply_type(Type, Data, Req, Env) ->
%%     {ok, Req1} = send_page(Type, Data, Req),
%%     {ok, Req1, Env}.

reply_html(Obj, Req, Env) ->
    {ok, Req1} = send_page(html, Obj, Req),
    {ok, Req1, Env}.

reply_json(Obj, Req, Env) ->
    {ok, Req1} = send_page(json, Obj, Req),
    {ok, Req1, Env}.

%%----------------------------------------------------------------------
%% websocket stuff

websocket_init(_Transport, Req, _Env) ->
         io:format("Initialising a web socket:(~p)(~p)(~p)",
		   [_Transport, _Env, path(Req)]),
    ["/", "websocket", ModStr] = path(Req),
    %% Args = args(Req),
    Req1 = cowboy_req:compact(Req),
    Self = self(),
    Mod = list_to_atom(ModStr),
    %% Spawn an erlang handler
    Pid = spawn_link(Mod, start, [Self]),
    {ok, Req1, Pid, hibernate}.

websocket_handle({text, Msg}, Req, Pid) ->
    %% This is a Json message from the browser
    case catch decode(Msg) of
	{'EXIT', _Why} ->
	    Pid ! {invalidMessageNotJSON, Msg};
	{struct, _} = Z ->
	    X1 = atomize(Z),
	    Pid ! {self(), X1};
	Other ->
	    Pid ! {invalidMessageNotStruct, Other}
    end,
    {ok, Req, Pid}.

websocket_info({send,Str}, Req, Pid) ->
    {reply, {text, Str}, Req, Pid, hibernate};
websocket_info([{cmd,_}|_]=L, Req, Pid) ->
    B = list_to_binary(encode([{struct,L}])),
    {reply, {text, B}, Req, Pid, hibernate};
websocket_info(Info, Req, Pid) ->
    io:format("Handle_info Info:~p Pid:~p~n",[Info,Pid]),
    {ok, Req, Pid, hibernate}.


websocket_terminate(_Reason, _Req, Pid) ->
    io:format("websocket.erl terminate:~n"),
    exit(Pid, socketClosed),
    ok.

%% reply_json(Obj, Req, Env) ->
%%     %% Encode Obj as JSON and send to the browser
%%     Json = encode(Obj),
%%     {ok, Req1} = send_page(json, Json, Req),
%%     {ok, Req1, Env}.

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

%% rpc(Pid, M) ->
%%     S = self(),
%%     Pid ! {S, M},
%%     receive
%% 	{Pid, Reply} ->
%% 	    Reply
%%     end.

%%----------------------------------------------------------------------
%% atomize turns all the keys in a struct to atoms

atomize({struct,L}) ->
    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.

%%----------------------------------------------------------------------
%% these are to be called from the gui client code

append_div(Ws, Div, X) ->
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,append_div},{id,Div},{txt,Bin}]).

fill_div(Ws, Div, X) ->
    io:format("websockets X=~p~n",[X]),
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,fill_div},{id,Div},{txt,Bin}]).
    

send_websocket(Ws, X) ->
    Ws ! {send, list_to_binary(encode([{struct,X}]))}.

terminate({normal,shutdown},_, _) ->
    true;
terminate(_Reason,_Req,_State) ->
    true.

    

read_latest(Env) ->
    F = os:getenv("HOME") ++ "/Dropbox/chandler/latest.j",
    %% F = Env#env.root ++ "/latest.j",
    %% {ok, [X]} = file:consult(F),
    %% B = list_to_binary(encode(X)),
    {ok, B} = file:read_file(F),
    io:format("read: ~s ~p bytes~n",[F, size(B)]),
    B.

write_latest(Bin) when is_binary(Bin)->
    F = os:getenv("HOME") ++ "/Dropbox/chandler/latest.j",
    ok = file:write_file(F, Bin),
    io:format("wrote:~s ~p bytes~n",[F, size(Bin)]).

read_latest_bin() ->
    {ok, B} = file:read_file("latest"),
    B.

index("collection_" ++ T) ->
    list_to_integer(T).

write_debug(Bin) ->
    Out = make_tmp_filename("./website/backup", 0),
    io:format("** dumping to ~s~n",[Out]),
    file:write_file("latest", Bin),
    file:write_file(Out, Bin).

make_tmp_filename(Root, N) ->
    Name = Root ++ "_" ++ integer_to_list(N),
    case filelib:is_file(Name) of
	true -> make_tmp_filename(Root, N+1);
	false -> Name
    end.

b2a(B) ->
    list_to_atom(binary_to_list(B)).

apply1(Mod, Func, Args) ->
    case apply(Mod, Func, Args) of
	{'EXIT', Why} ->
	    io:format("Error calling:~p:~p(~p) => ~p~n",[Mod,Func,Args,Why]),
	    <<"error">>;
	Json ->
	    case catch encode(Json) of
		{'EXIT', Why1} ->
		    io:format("Not a json term:~p~n", [Json]),
		    <<"error">>;
		Ret ->
		    Ret
	    end
    end.

		    
