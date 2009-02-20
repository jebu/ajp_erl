# leave these lines alone
.SUFFIXES: .erl .beam

.erl.beam:
	erlc -o $(EBIN_DIR) -pa $(EBIN_DIR) -W $<

SOURCE_DIR=src
EBIN_DIR=../ebin
INCLUDE_DIR=include
ERL = erl -o $(EBIN_DIR) -I ${INCLUDE_DIR}
SRC = src

MODS = ajp ajp_server gen_ajp_handler test_ajp_mount

all:	compile

compile: ${MODS:%=%.beam}

application: compile

run:
	${ERL} -pa ebin -s test test

# remove all the code
clean:
	rm -rf ../ebin/*.beam erl_crash.dump
