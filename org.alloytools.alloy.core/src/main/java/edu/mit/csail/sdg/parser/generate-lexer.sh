#!/bin/bash

java -cp $LIB_SDG/JFlex.jar JFlex.Main --nobak -d . Alloy.lex

sed -i 's/public Symbol next_token() throws java.io.IOException/public Symbol next_token() throws java.io.IOException, Err/' CompLexer.java

