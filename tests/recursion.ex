(*
func recursion(m,n);
  if (n >= m) then
    return recursion(m, n-1) - 1
  else
    return n;
  end
end;

recursion(6,1000);
*)


func fibonacci(n)
  if(n <= 1)
    return n
  else
    return (fibonacci(n - 1) + fibonacci(n - 2))
end;

print fibonacci(32)



