#!/usr/bin/env bash

#@ DESCRIPTION: print error msg and exit with a given ret value
die() #@ USAGE: die STAUTS [MESSAGE]
{
    error=$1
    shift
    [ -n "$*" ] && printf "%s\n" "$*" >&2
    exit "$error"
}

subreponame="TextToNumber"

if [ ! -d "./${subreponame}" ]
then
    git submodule init \
        && git submodule update
fi

if [ ! -e ./optional_dependencies/${subreponame}/dist/build/${subreponame}/${subreponame}.exe ]
then
    cd ./optional_dependencies/${subreponame} \
    && stack build \
    && stack install --local-bin-path . \
    && cd ..
fi

(test -e ./optional_dependencies/${subreponame}/${subreponame}.exe || test -e ./optional_dependencies/${subreponame}/${subreponame}) \
    || die 1 "Setup did not work: could not find binary files after trying to installing them"
