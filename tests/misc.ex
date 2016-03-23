var lst := []
for var i:=1000; i >= 0; i-- do
  lst += i;
end;

//print lst
var t := time
print lst
print time - t
