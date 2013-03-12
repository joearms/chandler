-module(web_book).
-compile(export_all).
%% JSON callable

new_book(B) ->
    X = binary_to_list(B),
    File = book_dir() ++ X ++ ".book",
    io:format("new_book:~p~n",[File]),    
    case filelib:is_file(File) of
	true ->
	    false;
	false ->
	    file:write_file(File,"abc"),
	    true
    end.

list_books() ->
    %% list_books knows where the books are stored
    io:format("list_books:~n"),
    Cmd = book_dir() ++ "*.book",
    L = filelib:wildcard(Cmd),
    io:format("Files:~s ~p~n",[Cmd,L]),
    L1 = [list_to_binary(filename:basename(I)) || I <- L],
    io:format("Files1:~p~n",[L1]),
    {struct, [{files,L1}]}.

book_dir() ->
    os:getenv("HOME") ++ "/published/chandler/data/".
    
    
