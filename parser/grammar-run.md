```
➜  parser git:(parser/antlr_grammar) ✗ make grammar grammarrun
java -jar /Users/alvaro/Documents/sandbox/haskell/haskell-simple-sessions/parser/antlr*.jar Grammar.g4
javac Grammar*.java
java org.antlr.v4.gui.TestRig Grammar stat -tree
F AND F;
T AND NOT F;
F OR NOT NOT F;
NOT F OR NOT F AND F;
line 5:0 no viable alternative at input '<EOF>'
(stat (expr (expr F) AND (expr F)) ; (expr (expr T) AND (expr NOT (expr F))) ; (expr (expr F) OR (expr NOT (expr NOT (expr F)))) ; (expr NOT (expr (expr F) OR (expr NOT (expr (expr F) AND (expr F))))) ; expr)
```
