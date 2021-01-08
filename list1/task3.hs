fib n = f n 0 1 where
  f 0 a b = a
  f 1 a b = b
  f n a b = f (n-1) (b) (a+b)

seq3 n = f n 1 1 2 where
  f 0 a b c = a
  f 1 a b c = b
  f n a b c = f (n-1) (b) (a+b+c) (c+1)