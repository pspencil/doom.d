#!/usr/bin/env bash
set -euo pipefail

if [[ -f ~/.emacs.d/bin/org-tangle ]]
then
    ~/.emacs.d/bin/org-tangle ~/.doom.d/other
else
    echo "Error: You must have doom emacs installed."
    exit 1
fi
