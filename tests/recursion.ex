(*
func fibonacci(n)
  //print n
  if(n = 0)
    return 0
  else if(n = 1)
    return 1
  else
    return (fibonacci(n - 1) + fibonacci(n - 2))
end;

print fibonacci(12)
*)


func recursion(n);
  if (n = 10) then
    var res := recursion(n-1) + recursion(n-2)
    print n //oh snap! we have changed `n` to `7`
    return res
  else
    return n;
  end
end;

print recursion(10);


