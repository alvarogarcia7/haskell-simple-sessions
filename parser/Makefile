.PHONY: test clean

test:
	cabal test

build:
	cabal build

clean:
	rm -f *.java *.tokens *.class
	cabal clean

grammar:
	java -jar ${PWD}/antlr*.jar Grammar.g4
	javac Grammar*.java

grammarrun:
	java org.antlr.v4.gui.TestRig Grammar stat -tree

tool:
	java org.antlr.v4.Tool Grammar.g4
