var c:=0;

func quicksort(a, ilo, ihi)
  c += 1;
  //if(c > 1000) return None;        
  var lo := ilo
  var hi := ihi
  var pivot := a[(lo + hi) div 2]
  repeat
    while(a[lo] < pivot) lo += 1
    while(a[hi] > pivot) hi -= 1
    if lo <= hi then
      var tmp := a[lo]
      a[lo] := a[hi]
      a[hi] := tmp
      lo += 1
      hi -= 1
    end
  until(lo > hi)
  if(hi > ilo) quicksort(a, ilo, hi)
  if(lo < ihi) quicksort(a, lo, ihi)
end

var lst := []
for var i:=100000; i >= 0; i-- do
  lst += i;
end;

//print lst
var t := time
quicksort(lst, 0,100000)
print time - t, c

//print lst
