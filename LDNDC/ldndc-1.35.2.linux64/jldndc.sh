#!/bin/sh

## execute from cmake_binary_dir

## TODO generate during install
java -Djava.library.path=lib -classpath lib/jldndc.jar:lib/jcal.jar  jldndc.jldndc

