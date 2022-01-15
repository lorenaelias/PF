raizes :: Float -> Float -> Float -> (Float,Float)         
raizes a b c = 
  if d<0 then error "raizes nao reais" else (((-b)+sqrt d)/(2*a),((-b)-sqrt d)/2*a)
  where {d = b*b-4*a*c}