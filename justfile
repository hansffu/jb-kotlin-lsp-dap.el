set positional-arguments

# Start a separate Emacs with this checkout loaded. Extra arguments go to Emacs.
run *args:
    @"${EMACS:-emacs}" -L . --eval "(require 'jb-kotlin-lsp-dap)" "$@"
