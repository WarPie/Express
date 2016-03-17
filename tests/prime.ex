var lim := 500 000
var primes := 0
var n := 1
while n < lim do
  var k := 3
  var is_prime := True
  n += 2
  while is_prime do
    if (k * k > n) break;
    is_prime := n div k * k != n
    k += 2
  end
  if(is_prime) primes += 1
end
