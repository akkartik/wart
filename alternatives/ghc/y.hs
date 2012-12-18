foo =
  do x <- [1,2,3]
     y <- [1,2,3]
     True <- return (x /= y)
     return (x,y)

foo2 a b = a
    >>= (\x -> b
        >>= (\y -> return (x/=y)
            >>= (\r -> case r of True -> return (x,y)
                                 _    -> fail "")))

data Foo = Num | Char


fib@(1:tfib)    = 1 : 1 : [ a+b | (a,b) <- zip fib tfib ]

foo3 =
  return 7 >>= (\x -> Writer (x+1,"inc."))
      >>= (\x -> Writer (2*x,"double."))
      >>= (\x -> Writer (x-1,"dec.")) 
