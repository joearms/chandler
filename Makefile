MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

## note this assumes that cowboy is somewhere in your path

all: beam
	erl -s erl3 start
 
##	cd ..; erl -pa src -s websockets start_link 2233 `pwd`		

beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam *.so *.o *~ erl_crash.dump



