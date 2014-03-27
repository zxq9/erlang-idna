all: clean compile

clean:
	@rm -rf ebin/*.beam

priv/UnicodeData.txt:
	@test -d priv || mkdir priv
	@curl http://www.unicode.org/Public/UNIDATA/UnicodeData.txt > priv/UnicodeData.txt

compile: priv/UnicodeData.txt
	@test -d ebin || mkdir ebin
	@erl -make

test: clean compile
	@escript test.escript
