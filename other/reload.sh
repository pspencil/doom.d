#!/usr/bin/env bash
set -euo pipefail

if [[ ! -f ~/.emacs.d/bin/org-tangle ]]
then
    echo "Error: You must have doom emacs installed."
    exit 1
fi

if [[ "$#" = 0 ]]
   then
       ~/.emacs.d/bin/org-tangle ~/.doom.d/other
else
    ~/.emacs.d/bin/org-tangle "$1"
fi

    
