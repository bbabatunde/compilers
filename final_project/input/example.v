class test:
                 fields x = 1, y = 2
                 methods
                 def f1():
                     1
                 def f2():
                     2
                 end

            let x = new test in
            let y = x
                 in  y.f2()