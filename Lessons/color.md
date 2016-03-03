Color Pictures
============================================================

Previously, we assigned to a two-input function `f:: R -> R -> R`, a picture. The amount of white at the point $(x,y)$ was proportional to the value `f x y`. We are now going to do the same, but for color pictures.

The idea is very simple, instead of $f(x,y)$ representing a single value, it will represent a triple of values, one for each basic color red, green, and blue. 
So, the values for $f$ need to be triples $(r,g,b)$, where $r$, $g$, and $b$ are real numbers. So, for example, the value $(1,0,0)$ will be purely red, $(0,1,0)$ green, $(0,0,1)$ blue. $(0,0,0)$ will be black (no color). $(1,1,1)$ will be white. $(0.5,0.5,0.5)$ will be gray. Other colors will be combinations of the pure colors. 

Such triples have type `(R,R,R)`. That means that we can draw color pictures for functions of the form `f :: R -> R -> (R,R,R)`.

Here is an example of such a function:

~~~haskell
theFunctionToDraw = f

f :: R -> R -> (R,R,R)
f x y = if x>0 then (x,0,0)
               else (0,-x,0)
~~~
It draws:

![](/image/lesson/color_01.jpeg "The image produced")

What's happening here? The vaue `f x y` of the function, when $x<0$ is `(x,0,0)`, which means that, when `x` is close to zero, it will be close to `(0,0,0)`, almost black. As `x` increases, the value of the function will get closer to `(1,0,0)`, which is the color red. Similarly, when $x\leq 0$, we start with black in the middle, and end up, at $x=-1$, with `(0,1,0)`, pure green. 


#### A more interesting example

Let's make things a little more interesting. Try to study the following code, and figure out what it will produce. 

~~~haskell
theFunctionToDraw = f

f :: R -> R -> (R,R,R)
f x y = (0, g x, g y)

g t = abs t
~~~
@@@ Show result

![](/image/lesson/color_01c.jpeg "The image produced")

What happened? `g t` is just the absolute value $|t|$, so the function is $f(x,y) = (0, |x|, |y|)$. This function has no red in it. The amount of blue depends on how far $y$ is from $0$. And the amount of green depends on how far $x$ is from $0$. Hence the picture, with a black middle, and greenish sides and blueish top and bottom. 
@@@

Surely you can play around, putting different functions into the red,green, blue parts and make something more interesting than these.

<br>
<!--Time to get serious with the [Level 2 Problems](/Lessons/level2problems). -->

<br>
<br>
