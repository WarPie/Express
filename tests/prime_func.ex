func prime(lim)
  var n := 1
  var primes := 0

  while(n < lim) do 
    var k := 3
    var is_prime := True
    n += 2
    while (is_prime) do
      if(k * k > n) break;
      is_prime := n div k * k != n 
      k += 2
    end
    if(is_prime) primes += 1
  end
  
  return n
end

print prime(500 000)
