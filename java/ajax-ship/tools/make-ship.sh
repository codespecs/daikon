#!/bin/sh

perl tools/addAllCopyrights.pl
zip -ur ajax-ship ajax/* -i '*.java'
zip -u ajax-ship reflection *.sal *.csal README
zip -u ajax-ship tools/*
