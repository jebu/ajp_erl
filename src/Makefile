
include ../../vsn.mk

EMULATOR=beam
EBIN=../ebin
INCLUDE=../include

APP_TARGET=$(EBIN)/ajp_app.app
APPUP_TARGET=$(EBIN)/ajp_app.appup

ERL_COMPILE_FLAGS += -I$(INCLUDE) -DTEST +warn_unused_vars +nowarn_shadow_vars +warn_unused_import +debug_info

SOURCES= \
  ajp.erl \
  ajp_server.erl \
  ajp_app.erl \
  ajp_sup.erl \
  gen_ajp_handler.erl \
  test_ajp_mount.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/ajp_records.hrl

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(APP_TARGET): ajp_app.app.src ../../vsn.mk
	sed -e 's;%VSN%;$(HARVESTER_VSN);' $< > $@

$(APPUP_TARGET): ajp_app.appup.src ../../vsn.mk
	sed -e 's;%VSN%;$(HARVESTER_VSN);' $< > $@
