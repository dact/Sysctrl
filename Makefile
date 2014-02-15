.PHONY : build clean

build:
	mkdir bin
	ghc src/Main.hs -isrc/ -o bin/sysctrl

clean:
	find src/ -regex ".*\.hi\|*\.o" -exec rm -f {} \;
	rm -fr bin/
