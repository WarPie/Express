var glob := 1000;

func bar(c)
  return c;
end;

func foo();
  var b := bar(100)
  
  func local(z)
    b := b div bar(z);
  end
  
  print b = 100;
  local(10);
  print b = 10;

  func local2()
    var b := 100;
    return b
  end
  
  print b = 10
  print local2() = 100
  print b = 10
  
  func not_glob()
    var old_glob := glob;
    var glob := 100;
    print old_glob != glob;
  end
  not_glob()
  
  var glob := glob;
  print glob = 1000
end;

foo()