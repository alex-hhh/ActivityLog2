#!/bin/bash

# run "raco check-requires" on some files.  Files and directories to check can
# be specified on the command line or as the -l option which only checks files
# modified by the last commit (or commits).

# Running check-requires on all the files in the project can take a very long
# time,  the travis build should be set up to check just the last commit.

FILTER=${0%/*}/filter-check-requires.awk

if ! [ -r "$FILTER" ]; then
    echo "Could not find filter script at $FILTER"
    exit 1
fi

NCOMMITS=1
MODE=all

while getopts :hl: OPT; do
    case $OPT in
        h|+h)
            echo "usage: ${0##*/} [+-hl] [--] ARGS..."
            echo ""
            echo "    -l NUM -- check requires for files touched by last NUM commits";
            echo ""
            exit 2
            ;;
        l|+l)
            MODE=commits
            NCOMMITS=$OPTARG
            ;;
        *)
            echo "usage: ${0##*/} [+-hl] [--] ARGS..."
            exit 2
    esac
done
shift $(( OPTIND - 1 ))
OPTIND=1

case $MODE in
    all)
        echo "Will check-requires in the following files and directories: $@"
        for item in $@; do
            if [ -f "$item" ]; then
                raco check-requires "$item" |\
                    awk -f $FILTER
            elif [ -d "$item" ]; then
                find "$item" -type f -name '*.rkt' -exec raco check-requires "{}" \; |\
                    awk -f $FILTER
            else
                echo "Skipping unknown item: $item"
            fi
        done
        ;;
    commits)
        echo "Will check-requires in files changed in the last $NCOMMITS commit(s)."
        git diff --numstat HEAD~$NCOMMITS HEAD |\
            awk '/\.rkt$/ { print $3; }' |\
            while read file
            do
                # NOTE: git diff will also list deleted files, don't check them
                if [ -f "$file" ]; then
                    raco check-requires "$file" |\
                        awk -f $FILTER
                fi
            done
        ;;
    *)
        echo "Unknown work mode"
        exit 2
esac
