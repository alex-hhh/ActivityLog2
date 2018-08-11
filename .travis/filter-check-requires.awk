
# Filter the output of the raco check-requires command, to remove some advice
# that we cannot do anything about and just generates noise.

BEGIN {
    file="";
    file_emitted = 0;
    skip_bypass = 0;

    # These are some modules that we don't want to bypass (that is replace
    # them with files imported by these and re-exported).  Put them here and
    # filter them out of the report.
    useless_bypass[0] = "typed/racket/base";
    useless_bypass[1] = "racket/contract";
    useless_bypass[2] = "widgets/main.rkt";
    useless_bypass[3] = "al-interactive.rkt";
    useless_bypass[4] = "xml";
}

# cancel skip bypass at the first line that is not a "TO statement
/^[^ ]/ { skip_bypass = 0; }

# skip all "TO" statements for a skipped bypass
/^  TO/ && skip_bypass { next; }

# If this is a BYPASS line, check for some things that are suggested, but we
# won't or can't bypass...
/^BYPASS/ {
    match($0, /^BYPASS "?(..\/)*([^"]*)"? at/, result);
    for (i in useless_bypass) {
        if (result[2] == useless_bypass[i]) {
            skip_bypass = 1;
            next;
        }
    }
}

# If this is a "file" line, save the file name, but delay outputting it until
# there is some message about the requires in this file, we don't want to
# print out files for which there is no action to take...
/^\(file "(.*)"\):/ {
    match($0, /^\(file "(.*)"\):/, result);
    file = result[1];
    file_emitted = 0;
    next;
}

# Skip DROP instructions for submodules, and some other useless advice...
/^DROP \(submod.*)/ { next; }
/^DROP typed-racket\/utils\// { next; }

# plot-hack and fmt-util cannot be dropped, not sure why raco check-requires
# suggests that they are dropped...
/^DROP .*plot-hack\.rkt/ { next; }
/^DROP .*fmt-util\.rkt/ { next; }

{
    # omit empty lines, but otherwise print the line.  Also print the file
    # name if we delayed outputting it.
    if (length($0) > 0) {
        if (! file_emitted) {
            printf("\n%s :\n", file);
            file_emitted = 1;
        }
        print;
    }
}
