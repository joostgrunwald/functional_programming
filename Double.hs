
--normal order?
double x = incr (incr 0)
where incr y = x + y

--double 5 = incr (incr 0)          (fill in parameter)
--where incr y = 5 + y              (fill in parameter)

--double 5 = 5 + incr 0             (expand incr)
--double 5 = 5 + 5 + 0              (expand incr)
--double 5 = 10 + 0                 (simplify)
--double 5 = 10                     (simplify)