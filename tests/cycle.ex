(*
x := 0.0;
while(x < 10000000) do
  x += 1;
end;
*)

var lst1,lst2 := [];
for var i:=0; i < 100000; i++ do
  lst1 := [1,2,3,4,5,i];
  lst1 += lst1;
  lst2 := lst1;
  lst1 := None;
end;
print lst2
