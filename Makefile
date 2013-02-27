all:
	@test -d deps || rebar get-deps	
	rebar compile
	erl -pa './deps/cowboy/ebin' -pa './deps/ranch/ebin' -pa './ebin' -s ws start 2007 


