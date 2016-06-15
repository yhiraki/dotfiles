#!/bin/bash

LIB_JAVA_DIR=$HOME/lib/java

if [ ! -d $LIB_JAVA_DIR ]; then
  mkdir -p $LIB_JAVA_DIR
fi

wget http://downloads.sourceforge.net/project/plantuml/plantuml.jar -O $LIB_JAVA_DIR/plantuml.jar
