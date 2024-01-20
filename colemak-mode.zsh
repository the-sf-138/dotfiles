#!/usr/bin/env zsh

typeset -A qwerty_to_colemak
qwerty_to_colemak=(
    "f" "e"
    "p" "r"
    "g" "t"
    "j" "y"
    "l" "u"
    "u" "i"
    "y" "o"
    ";" "p"
    "r" "s"
    "s" "d"
    "t" "f"
    "d" "g"
    "n" "j"
    "e" "k"
    "i" "l"
    "o" ";"
    "k" "n"
    "." ","
    "," "."
    "F" "E"
    "P" "R"
    "G" "T"
    "J" "Y"
    "L" "U"
    "U" "I"
    "Y" "O"
    ":" "P"
    "R" "S"
    "S" "D"
    "T" "F"
    "D" "G"
    "N" "J"
    "E" "K"
    "I" "L"
    "O" ":"
    "K" "N"
)

typeset -A original_binding

for ckey qkey in ${(kv)qwerty_to_colemak}; do
    original_binding[$qkey]=$(bindkey -M vicmd $qkey | awk '{print $2}')
done

for ckey qkey in ${(kv)qwerty_to_colemak}; do
    bindkey -M vicmd $ckey $original_binding[$qkey]
done
