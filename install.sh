#!/usr/bin/env sh

FAILED_EMOJI="\e[31m\u274C\e[0m"
SUCCESS_EMOJI="\e[32m\u2713\e[0m"

# Doom
if [[ $(ls $DOOMDIR | wc -l) -gt 0 ]]; then
    read -p "Clean up $DOOMDIR? [y/n] " confirmation

    if [[ "$confirmation" = "y" ]]; then
        rm $DOOMDIR/*.el
    fi
fi

for f in "doom/packages.el" "doom/config.el" "doom/init.el"; do
    MSG="install of ${f}:\t "
    ln -s $(pwd)/${f} $DOOMDIR/$(basename ${f}) && echo -e "$MSG $SUCCESS_EMOJI" || echo -e "$MSG $FAILED_EMOJI"
done

if [[ $(ls ~/.zshrc | wc -l) -gt 0 ]]; then
    read -p "Clean up ~/.zshrc? [y/n] " confirmation
    if [[ "$confirmation" = "y" ]]; then
        rm ~/.zshrc
    fi
fi

for f in ".zshrc" ".gdbinit"; do
    MSG="install of ${f}:\t "
    ln -s $(pwd)/${f} ~/${f} && echo -e "$MSG $SUCCESS_EMOJI" || echo -e "$MSG $FAILED_EMOJI"
done

