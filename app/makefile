BIN=fortran-amedas
MKFILE:=-C .. -f makefile

run: clean release
	../build/app/$(BIN) \
  --dir "/mnt/DATA/amedas"

release:
	make release $(MKFILE)

test:
	make test $(MKFILE)

install:
	make install $(MKFILE)

uninstall:
	make uninstall $(MKFILE)

clean:
	make clean $(MKFILE)
