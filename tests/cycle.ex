(*
x := 0.0;
while(x < 10000000) do
  x += 1;
end;
*)


var x := 'Hello World';
x[5] := '_';
print x;

var lst1 := None;
for var i:=0; i < 100000; i++ do
  lst1 := [lst1, [#112, 2*2, 3.0/2], None, False=True];
end;

print lst1[0][0][0][0][0][0][0][0][0][0][1]