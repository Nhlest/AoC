cat aoc03.input | awk '{for(c=0;c<10;c++) {$0=$0$0}; print substr($0, NR*3+1, 1)}' | sort | uniq -c | awk '/\./ {print $1}'
