def readProblem (input : String) : List String :=
  input.splitOn "\n"
    |>.filter (·.length > 0)  -- Remove empty lines
