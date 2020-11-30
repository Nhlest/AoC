function sample_function() {
  local x i less=() more=()
  result=()
  (($#==0)) && return 0
  x=$1
  shift
  for i; do
    if (( i < x )); then
      less+=( "$i" )
    else
      more+=( "$i" )
    fi
  done
  sample_function "${less[@]}"
  less=( "${result[@]}" )
  sample_function "${more[@]}"
  more=( "${result[@]}" )
  result=( "${less[@]}" "$x" "${more[@]}" )
}
