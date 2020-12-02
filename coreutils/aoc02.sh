cat aoc02.input | awk '
BEGIN {total=0}
{
  split($1,bounds,"-"); 
  char=substr($2,length($2)-2,length($2)-1); 
  pw=$3; 
  split(pw, pwa, "");
  count=0;
  for(i in pwa) {
    if (pwa[i]==char) {count=count+1}
  }
  if(count>=bounds[1] && count<=bounds[2])
    {total++}
}
END{print(total)}'
