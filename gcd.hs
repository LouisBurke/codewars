module GCD where
import Prelude hiding (gcd, lcm)

gcd' :: Integral n => n -> n -> n -> n
gcd' a b g
  | and [even a, even b] = gcd' (div a 2) (div b 2) g*2
  | and [even a, odd b] = gcd' (div a 2) b g
  | and [even b, odd a] = gcd' a (div b 2) g
  | a > b = gcd' (div (a-b) 2) b g
  | a < b = gcd' a (div (b-a) 2) g
  | a == b = a*g

gcd :: Integral n => n -> n -> n
gcd a b = gcd' a b 1
