#!/bin/sh

mv site/src .
ln -s site/src ../src
mv site/include .
ln -s site/include ../include
rm -fr site/ebin
rm -fr releases
rm -fr fix-slim-release
rm -fr lib
mv bin/nitrogen bin/nitrogen.old
mv bin/dev bin/dev.old
mv Makefile Makefile.old

## Download new Makefile

## download rebar3

## download assemble_config.escript

## download new nitrogen/bin

## rewrite rebar.config based on current deps
