#/bin/bash

# Create a TAGS table for this project.  For larger projects such as this one,
# TAGS works better for navigations, as files don't have to be
# evaluated/expanded/check-syntaxed first.  Also, this script will create TAGS
# entries for the tables and views in the database schema, so you can navigate
# from a SQL query in Racket to the table definition.

/bin/rm TAGS > /dev/null 2>&1

ETAGS=d:/emacs/bin/etags.exe

find . -type f -name '*.rkt' | \
    $ETAGS --language=scheme -

# Pick up tags from the database schema, so we can navigate to database tables
# as well

find . -type f -name 'db-schema.sql' | \
    $ETAGS --append \
           --regex="/create[ \t]+\(\(table\|view\|unique\|index\)[ \t]+\)*\(?3:[^ \t(]+\)/\3/i" \
           -

