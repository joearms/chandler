-module(erl3_webserver).
-compile(export_all).
-import(mochijson2, [encode/1, decode/1]).

-export([start_link/1,
	 start_link/2,
	 start_embedded/1,
	 init/3,      websocket_init/3,
	 handle/2,    websocket_handle/3, 
	 terminate/2, websocket_terminate/3,
	 websocket_info/3,
	 append_div/3,
	 pre/1,
	 fill_div/3,
	 terminate/3
	]).

%% env has only one parameter - reserved for future expansion

-record(env, {dispatch}).

start_embedded(Port) ->
    ok   = application:start(ranch),
    ok   = application:start(cowboy),
    web_server_start(Port, "zip"),
    receive
	after 
	    infinity ->
		true
	end.

start_link([PortAtom, DirAtom]) ->
    Port = list_to_integer(atom_to_list(PortAtom)),
    Dir  = atom_to_list(DirAtom),
    io:format("Starting server on port:~p Dir:~p~n",[Port,Dir]),
    start_link(Dir, Port).

start_link(Dispatch, Port) ->
    ok = application:start(crypto),
    ok = application:start(ranch),  
    ok = application:start(cowboy),
    ok = web_server_start(Port, Dispatch),
    receive
	after 
	    infinity ->
		true
	end.
    
web_server_start(Port, Dispatcher) ->
    E0 = #env{dispatch=Dispatcher},
    Dispatch = cowboy_router:compile([
     				      {'_', 
 				       [
    					{'_', ?MODULE, E0}
    				       ]}
    				     ]),  
    
    %% server is the name of this module
    NumberOfAcceptors = 100,
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
	    io:format("websockets started on port:~p~n",[Port])
    end.

init(_, Req, E0) ->
    io:format("Init~n"),
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

terminate(_, _) ->  
    ok.
    
handle(Req, Env) ->
    Resource = filename:join(path(Req)),
    io:format("~s:handle ~p~n",[?MODULE, Resource]),
    handle1(Resource, Req, Env).

unhex("%22" ++ T) -> [$"|unhex(T)];
unhex("%20" ++ T) -> [$\s|unhex(T)];
unhex([H|T])      -> [H|unhex(T)];
unhex([])         -> []. 

handle1("/write_json_file", Req, Env) ->
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
handle1("/read_json_file", Req, Env) ->
    [{<<"filename">>,FB}] = args(Req),
    Filename = binary_to_list(FB),
    {ok, [X]} = file:consult(Filename),
    B = encode(X),
    reply_html(list_to_binary(B), Req, Env);
handle1("/read_latest", Req, Env) ->
    Bin = read_latest(Env),
    reply_html(Bin, Req, Env);
handle1("/store_data", Req, Env) ->
    %% io:format("extracting args:~p~n",[Req]),
    {Q,_} = cowboy_req:qs(Req),
    %% io:format("Q:~p~n",[Q]),
    Q1 = unhex(binary_to_list(Q)),
    %% io:format("Q1:~p~n",[Q1]),
    Args = (catch decode(Q1)),
    dump_json(Args),
    %% Args = args(Req),
    %% io:format("Store:~p~n",[Args]),
    dump_data(Q1),
    reply_html("ok", Req, Env);
handle1(Resource, Req, Env) ->
    F = Env#env.dispatch,
    File = F(Resource),
    io:format("MMmapped to:~p (isdir=~p)~n",[File,filelib:is_dir(File)]),
    case filelib:is_dir(File) of
	true ->
	    list_dir(File, Req, Env);
	false ->
	    Ext = filename:extension(File),
	    io:format("extension=~p~n",[Ext]),
	    case known_file_type(Ext) of
		true ->
		    serve_file(File, Req, Env);
		false ->
		    serve_wierdo_file(Ext, File, Req, Env)
	    end
    end.

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
classify_extension(".swf") -> swf;

classify_extension(_)      -> html.

known_file_type(X) ->
    lists:member(X, [".mp3", ".swf",
		     ".ico",".html", ".js", ".jpg", ".css", ".png", ".gif"]).


mime_type(ico)     -> "image/x-icon";
mime_type(gif)     -> "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
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


terminate(A,B,C) ->
    io:format("terminate:~p~n",[A]).

read_latest(Env) ->
    F = Env#env.dispatch,
    F1 = F("/latest.j"),
    {ok, [X]} = file:consult(F1),
    B = encode(X),
    io:format("send:~p~n",[B]),
    B.


read_latest_bin() ->
    {ok, B} = file:read_file("latest"),
    B.

index("collection_" ++ T) ->
    list_to_integer(T).

dump_data(Bin) ->
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

dump_json(X) ->
    {ok, S} = file:open("./website/latest.j", [write]),
    io:format(S, "~p.~n",[X]),
    file:close(S).

