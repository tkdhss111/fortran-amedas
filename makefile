CMAKE       := cmake -GNinja .. -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON
CMAKE_RLS   := $(CMAKE) -DCMAKE_BUILD_TYPE=Release
CMAKE_DBG   := $(CMAKE) -DCMAKE_BUILD_TYPE=Debug
MKDIR_BUILD := mkdir -p build && cd build

.PHONY: test install doc

run: clean release
	./build/app/fortran-amedas \
  --dir "/mnt/DATA/amedas"

release:
	$(MKDIR_BUILD) && $(CMAKE_RLS) && ninja

debug:
	$(MKDIR_BUILD) && $(CMAKE_DBG) && ninja

test: 
	$(MKDIR_BUILD) && $(CMAKE_DBG) && ninja && ctest -VV

valgrind: debug
	valgrind --trace-children=yes --track-fds=no --track-origins=yes --leak-check=full --show-leak-kinds=all --show-reachable=no ~/gdb/fortran_debug

install:
	cd build && ninja install

uninstall:
	cd build && xargs rm < install_manifest.txt

clean:
	rm -r build

doc:
	rm -r doc build; ford ford.md

