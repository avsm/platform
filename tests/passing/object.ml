let x =
  object
    inherit foo
    method bar = _
  end

class foo =
  object
    method x = 2
    inherit bar
  end

class foo =
  object(this)
    inherit bar
  end
