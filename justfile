set positional-arguments

# Start a separate Emacs with this checkout loaded. Extra arguments go to Emacs.
run *args:
    @"${EMACS:-emacs}" --no-init-file --no-splash -L . -l dev/init.el "$@"
