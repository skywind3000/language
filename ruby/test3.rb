
def foo(x)
  return proc {
    puts x
    x += 1
  }
end


f1 = foo(100)
f2 = foo(200)

f1.call()
f1.call()
f1.call()
f2.call()

