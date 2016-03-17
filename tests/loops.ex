(*
for(i:=0; i < 10; i++) do
  if (i=2) continue;
  print i;
end;

for(i:=0; i < 3; i++) do
  print i;
  for(j:=0; j < 5; j++) do
    if(j = 1) 
      break;
    print j;
  end;
end;
*)

var k := 0;
while(k < 1000000) k += 1;