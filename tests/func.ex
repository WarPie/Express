func get_sqr_func()
  func get_it() 
    func sqr(x) return x*x
    return sqr
  end
  return get_it;
end

print get_sqr_func()()(10)
var sqr := get_sqr_func()();

var i := 0;
var tt := time;
while(i < 1000000) i += 1
tt := time - tt

i := 0;
var t := time;
while i < 1000000 do (* slow as shit, but whatever... *)
  var x := sqr(i)
  i += 1
end;
print (time - t) - tt




