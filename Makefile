#==============================================================================
# Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
#
# This file is part of jhn_stdlib_props.
#
# jhn_stdlib_props is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# jhn_stdlib_props is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with jhn_stdlib_props.  If not, see <http://www.gnu.org/licenses/>.
#==============================================================================

REBAR="./rebar"
UUT=`ls deps/jhn_stdlib/src/*.erl | awk '{print$0}'`
.PHONY: setup compile retest test clean clean-test get-deps update-deps real-clean

test: clean-test setup compile
	@$(REBAR) -jk eunit skip_deps=true

retest: update-deps test

setup: get-deps
	cp ${UUT} src

rebar:
	mkdir -p deps
	(cd deps && git clone https://github.com/rebar/rebar)
	(cd deps/rebar && ./bootstrap)
	cp deps/rebar/rebar .

compile: rebar
	@$(REBAR) -j compile

clean: rebar
	@$(REBAR) -j clean

clean-test: rebar
	@rm -rf .eunit
	@rm -rf ebin
	@rm -rf src/*.erl
	@$(REBAR) -j clean skip_deps=true

dist-clean: clean-test
	@$(REBAR) -j delete-deps

real-clean: dist-clean
	rm -f rebar
	rm -fr deps

get-deps: rebar
	@$(REBAR) -j get-deps

update-deps: rebar
	@$(REBAR) -j update-deps
	@$(REBAR) -j get-deps
