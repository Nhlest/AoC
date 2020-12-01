cat aoc01.input | awk '{print (2020 - $1)}' | cat aoc01.input - | sort -n | uniq -d | dc -e '??*p'
