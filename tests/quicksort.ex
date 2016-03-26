
func quicksort(a, ilo, ihi)  
  var lo := ilo
  var hi := ihi
  var pivot := a[(lo + hi) div 2]
  var tmp := 0;
  while True do
    while(a[lo] < pivot) lo += 1
    while(a[hi] > pivot) hi -= 1
    if lo <= hi then
      tmp := a[lo]
      a[lo] := a[hi]
      a[hi] := tmp
      //a[lo],a[hi] := a[hi],a[lo] (* might be doable *)
      lo += 1
      hi -= 1
    end
    if (lo > hi) break;
  end
  if(hi > ilo) quicksort(a, ilo, hi)
  if(lo < ihi) quicksort(a, lo, ihi)
end

var lst := []
for var i:=100000; i >= 0; i-- do
  lst += i;
end;

var t := time
quicksort(lst, 0,100000)
print time - t
