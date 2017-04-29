class A
  def m1
    self.m2()
  end
  def m2
  end
end
module M
  def m3
    self.m4()
  end
end
class B < A
  def m2
  end
end
class C < A
  include M
  def m4
  end
end
class D < B
  include M
end 


B.new.m1
C.new.m1
C.new.m3
D.new.m1
B.new.m3
D.new.m3 
