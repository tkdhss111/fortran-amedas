CMAKE       := cmake -GNinja .. -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON
CMAKE_RLS   := $(CMAKE) -DCMAKE_BUILD_TYPE=Release
CMAKE_DBG   := $(CMAKE) -DCMAKE_BUILD_TYPE=Debug
MKDIR_BUILD := mkdir -p build && cd build

.PHONY: release
release:
	$(MKDIR_BUILD) && $(CMAKE_RLS) && ninja

.PHONY: debug
debug:
	$(MKDIR_BUILD) && $(CMAKE_DBG) && ninja

.PHONY: test
test: 
	$(MKDIR_BUILD) && $(CMAKE_DBG) && ninja && ctest -VV

.PHONY: valgrind
valgrind: debug
	valgrind --trace-children=yes --track-fds=no --track-origins=yes --leak-check=full --show-leak-kinds=all --show-reachable=no ~/gdb/fortran_debug

.PHONY: install
install:
	cd build && ninja install

.PHONY: uninstall
uninstall:
	cd build && xargs rm < install_manifest.txt

.PHONY: clean
clean:
	rm -r build

.PHONY: clean_build
clean_build:
	find . -type d -iname build | xargs rm -rf

.PHONY: doc
doc:
	rm -r doc build; ford ford.md

