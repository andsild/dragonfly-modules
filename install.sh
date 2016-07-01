#!/bin/bash

subreponame="TextToNumber"

if [ ! -d "./${subreponame}" ]
then
    git submodule init \
        && git submodule update
fi

if [ ! -e ./${subreponame}/dist/build/${subreponame}/${subreponame}.exe ]
then
    cd ${subreponame} \
    && cabal install \
    && cd ..
fi

test -e ./${subreponame}/dist/build/${subreponame}/${subreponame}.exe || die "Setup did not work. Try cloning submodule manually and run cabal install (see source code for script)"

pip install -r requirements.txt



#@ DESCRIPTION: print error msg and exit with a given ret value
die() #@ USAGE: die STAUTS [MESSAGE]
{
    error=$1
    shift
    [ -n "$*" ] && printf "%s\n" "$*" >&2
    exit "$error"
}
