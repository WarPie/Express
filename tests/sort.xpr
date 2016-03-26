//DWS (SCAR): used 3350ms (could be a debug build?)
//Express   : used 1420ms
//PYTHON    : used 980ms
//LAPE      : used a whopping 550ms
//PYPY      : used a whopping 350ms (jit might struggle with all this branching + lists)

func sort(arr,size)
  var gap := 0;
  while (gap < (size-1) div 3) gap := gap * 3 + 1;
  
  while(gap >= 1) do 
    for(var i:=gap; i < size; i++) do
      var j := i;
      while (j >= gap) do
        if (arr[j] >= arr[j-gap]) break; //we lack short-circuit eval
        var tmp := arr[j];
        arr[j] := arr[j-gap]
        j -= gap
        arr[j] := tmp
      end;
    end;
    gap /= 3;
  end;
  return arr
end

//just fill it with a lot of junk 
func new_test(n)
  var seed := 6239351; 
  var result := [54351];
  for (var i:=0; i<n; i++) do
    seed *= 9531;
    seed := seed & 16777215
    seed /= 6719;
    seed *= seed;
    seed := seed & 16777215;
    result += seed;
    seed := seed & 16777215;
    if (seed < 111) seed += 111;
  end;
  return result
end

func is_sorted(a,n)
  for(var i:=0;i<n-1;i++) 
    if(a[i] > a[i+1]) return False
  return True
end

var arr := new_test(300000)

var t := time;
arr := sort(arr, 300000)
print time - t;

print is_sorted(arr, 300000)
