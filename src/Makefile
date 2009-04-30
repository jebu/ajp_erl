# leave these lines alone
.SUFFIXES: .erl .beam

INCLUDE_DIR=../include
.erl.beam:
	erlc -I$(INCLUDE_DIR) -o $(EBIN_DIR) -pa $(EBIN_DIR) -W $<

SOURCE_DIR=src
EBIN_DIR=../ebin
ERL = erl -o $(EBIN_DIR) -I ${INCLUDE_DIR}
SRC = src

MODS = ajp ajp_server ajp_app ajp_sup gen_ajp_handler test_ajp_mount

all:	compile

compile: ${MODS:%=%.beam}

application: compile

run:
	${ERL} -pa ebin -s test test

# remove all the code
clean:
	rm -rf ../ebin/*.beam erl_crash.dump
