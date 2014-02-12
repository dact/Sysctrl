clean:
	find src/ -regex ".*\.hi$\|*\.o$" -exec rm -f {} \;
