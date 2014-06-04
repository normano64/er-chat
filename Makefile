#################### 
GROUP_NUMBER := 11
####################

ERLC := erlc #This is the standard Erlang compiler.
ERLC_FLAGS := -W -I include

ERL_FILES := $(wildcard src/*.erl) #Creates a list of .erl files from source.
BEAM_FILES := $(patsubst src/%.erl,ebin/%.beam,${ERL_FILES}) #Creates a list of .beam files by replacing the .erl by .beam in src.

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES)) #Filters out all test-functions from the ERL_FILES.
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))] #Creates a punctuated list from EDOC_SRC.

REQUIRED_DIR_NAME := er-chat #Creates pop_2014_project_group_11

PROJECT_DIR := $(notdir $(shell pwd)) #Creates a path to the project.

USER=$(shell whoami) #The username of the current user.
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H:%M:%S")__.tar.gz #Generate an Archive name.
ARCHIVE_DIR := .. 
ARCHIVE_NAME2 := OSM_2014_group_11_final_deliverable__$(shell date "+%Y-%m-%d__%H:%M:%S")__tar.gz
all: $(BEAM_FILES) #Just creating the .beam files? Shouldent this be ERL_FILES

ebin/%.beam: src/%.erl #Some sort of a compilation?
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

start: all 
	(cd ebin && erl -eval 'foo:start(), init:stop()')

test: all
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc: $(BEAM_FILES) 
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc/html'},{private,true}])" -s init stop

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*.beam
	rm -fr ebin/Mnesia*
	rm -fr Mnesia*
	(cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/* doc/html/*

archive: clean
	(cd $(ARCHIVE_DIR) && tar -zcvf "$(ARCHIVE_NAME2)" --exclude ".git*" --exclude "*~" --exclude "*#*" --exclude "*.DS*" --exclude "*.pm*" er-chat )
