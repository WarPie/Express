(*
  Sample file that contains a bit of this and a bit of that...
*)
print "Let's do this shit!!"
func prime(lim)
  var n := 1
  var primes := 0
  while(n < lim) do 
    var k := 3
    var is_prime := True 
    n += 2
    while (is_prime and k * k <= n) do 
      is_prime := n div k * k != n 
      k += 2
    end
    if(is_prime) primes += 1
  end
  return n
end

var i:=0;

func sqr(x) return x*x
for i:=0; i<5; i++ do
  print sqr(i)
end;
print '-----------------------'


i := 0.0
while (i++ < 3) print sqr(-i);
print '-----------------------'


var myPrimeFunc := prime
print myPrimeFunc(5000)
print '-----------------------'


var myfoo := None
if(myfoo) print 'foo!'; else print 'no foo!' 


myfoo := not myfoo
if not myfoo then
  print 'no foo'
else 
  print 'foo!'
end

for var i:=0; i < 5; i++ do
  var foo := 10;
  print foo;
  var foo := False; //redeclare
  print foo;
  foo := True;
  print foo;
end;