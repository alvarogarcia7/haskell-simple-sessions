.PHONY: test clean

test:
	cabal test

build:
	cabal build

clean:
	rm -f *.java *.tokens *.class
	cabal clean

grammarinstall:
	export CLASSPATH=".:${PWD}/antlr-4.5.3-complete.jar:${CLASSPATH}"

grammar:
	echo "ensure that you have run DOLLAR (make grammarinstall) before this"
	java -jar ${PWD}/antlr*.jar Grammar.g4
	javac Grammar*.java

grammarrun:
	echo "ensure that you have run DOLLAR (make grammarinstall) before this"
	java org.antlr.v4.gui.TestRig Grammar stat -tree

tool:
	java org.antlr.v4.Tool Grammar.g4
