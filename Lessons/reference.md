Quick Reference
============================================================

Here is a haskell file you can have around to review Haskell's syntax.

#####Basic syntax

~~~haskell
-- this line is a comment, it will be ignored by the code

f :: R -> R                   -- f is a function that takes in real numbers
                              -- and gives back real numbers

f x = x*x + (x-1)/x + x^2     -- arithmetic operations
g x = (-3)*x                  -- always put parantheses around negative numbers

h x = 2*(g x) + (f x)         -- you can re-use your own functions

m = 5
ff x = m*x                    -- you can define and re-use numbers too
~~~

#####Standard functions

~~~haskell
floor :: R -> Int           -- floor x = integer part of x
round :: R -> Int
fromIntegral :: Int -> R    -- turn an integer into a real number

abs :: R -> R               -- absolute value

sin :: R -> R
cos :: R -> R

sqrt :: R -> R              -- square root
pi :: R                     -- not a function, just a number
~~~

#####Pritority of evaluation: 

- function application is always first,
- then exponentiation, multiplication, addition,... just like math

when in doubt, use parantheses: `(sin (2*pi*x))*(cos ((-1)*x))`.  

#####Composing functions

~~~haskell
f :: R -> R
f x = sin x 

g :: R -> R
g x = pi*x

h x = f (g x)                 -- first apply g, then apply f
h x = (f . g) x               -- another way to write composition
h = f . g                     -- another way to define the same h

-- useful example:
rfloor :: R -> R
rfloor x = fromIntegral (floor x)     -- look at: types of fromIntegral and floor,
                                      -- source and target match. Good. 
~~~

#####More syntax

~~~haskell
stepFunction :: R -> R
stepFunction x = if x >= 0 then 1 else 0

-- other comparisons: >, <, <=, >=, == (is it equal?), /= (not equal) 

-- double step function
stepFunction' :: R -> R
stepFunction' x             -- note: no equals sign here!
  | x>= 1     = 2 
  | x>= 0     = 1           -- second line considered if first line not true
  | otherwise = 0           -- knows to use `otherwise` if previous lines fail

-- and
f :: R -> R
f x = if (x<1) && (x>0) 
        then 1 
        else 0

-- or and not
g :: R -> R
g x = if not (x>=1 || x<=0)           -- g is actually the same as f
  then 1 
  else 0        
~~~

#####Useful: `let-in` combo, `where`

~~~haskell

f x = let t = x + 1
          z = t^2       -- alignment is important here!
      in z + t

f x = z + t                     -- same function
        where t = x + 1       
              z = t^2           -- alignment!
~~~


#####Two-input functions

~~~haskell
-- standard two-variable functions
mod :: Int -> Int -> Int  -- takes two ints, gives one int
   -- to use, write:     mod x 2     (gives remainder of division)
   -- or write:          x `mod` 2                

(+) :: R -> R -> R        -- notation is different if the name is a symbol
   -- to use, write:    x + y
   -- or write:         (+) x y

-- defining your own multi-variable functions
f:: R -> R -> R
f x y = sqrt (x^2 + y^2)
     -- if I write:     x `f` y
     -- it means:       f x y
~~~


#####Useful example
~~~haskell
steps :: Int -> R -> R
steps k x 
  | x>1    = 1
  | x<0    = 0
  | otherwise   = (rfloor (x*k'))/k'
        where k' = fromIntegral k

rfloor :: R -> R
rfloor x = fromIntegral (floor x)
              
~~~

#####Functions can take functions as input!

~~~haskell
applyItTwice :: (R->R) -> (R->R)    -- takes a function, gives back a function
applyItTwice f x = f (f x)          

rotateLeft :: (R -> R -> R) -> (R -> R -> R)
rotateLeft f x y = f (-y) x

~~~



#####Pairs type

~~~haskell
-- (A,B) means pairs (x,y) with x of type A and y of type B

rep :: R -> (R,R)
rep x = (x,x)

swp :: (R,R) -> (R,R)
swp (x,y) = (y,x)

invertColor :: (R,R,R) -> (R,R,R) 
addThem (x,y,z) = (1-x,1-y,1-z)

~~~

<br><br>



