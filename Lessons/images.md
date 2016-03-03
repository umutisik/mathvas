Making Images
============================================================

We will now look at how to make a picture from a function. For this mode, you select either 'Grayscale' or 'Images' in the 'New' menu. Let's start with grayscale images.

For grayscale, the function to be drawn has type `f:: R -> R -> R`. 
For example:

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = f

f :: R -> R -> R
f x y = x^2+y^2 

~~~

`f` takes in two parameters, these are the first two `R`'s there. The number produced corresponds to the color of the pixel at the $(x,y)$ coordinates. Values `0` or smaller are is black, and `1` or larger makes the pixel white. Values in between are shades of gray. 

This is what we get if we hit 'Run' with this code.

![](/image/lesson/images_01.jpeg "The image produced")

What is going on:

* the function $f(x,y) = x^2 + y^2$ is the square of the distance from a point to the origin. `f x y = sqrt (x^2 + y^2)` would have been the actual distance to the origin. 
* The origin $(0,0)$ is the middle of the picture. $f(0,0) = 0$, which corresponds to the color black. 
* The right side is $x=1$, left side is $x=-1$. The top is $y=1$, and the bottom is $y=-1$. 
* As $(x,y)$ gets farther and farther from the origin, $f(x,y)$ is getting closer to $1$, making the color more and more white. 
* When $x^2 + y^2\geq 1$, i.e. outside the circle, $f(x,y)>1$, which gets truncated to white.

<br>
**Exercise** Let's try to guess what we are going to get if we run this code?

~~~haskell
theFunctionToDraw = f

f :: R -> R -> R
f x y = x
~~~
Really think about what color each pixel is going to be. What's the value at $(0,0)$? What's the value at $(1,0)$, $(-1,0)$, $(0,-1)$, $(0,1)$ and in between?

@@@ Show Result
![](/image/lesson/images_02.jpeg "The image produced")

Did you guess right? $f(x,y) = x$, so the $y$ coordinate has no effect on the function's value. $f(0,0) = 0$, $f(1,0) = 1$, so the function goes from black to white from the middle to the right. On the other hand, if $x<0$, then $f(x,y)< 0$, so the color is black on the left side altogether.
@@@

####Some Examples

**1.** How would we make this picture? 

![](/image/lesson/images_03.jpeg "The image produced")

Think about it, what does your `f` need to satisfy? what should `f 0 0` be? What about `f 0 1`, `f 1 0`,... When you figure it out, go ahead and try it in a new 'Grayscale' page.
@@@ Show Answer

~~~haskell
theFunctionToDraw = f

f :: R -> R -> R
f x y = (y+1)/2
~~~
@@@
 
<br>
**2.** 

![](/image/lesson/images_04_upperhalf.jpeg "The image produced")

Same deal. Don't forget to get the colors right. $0$ is black, $1$ is white.  

@@@ Show Answer

~~~haskell
theFunctionToDraw = f

f :: R -> R -> R
f x y = if y>x 
          then 0 
          else 1
~~~

Alternatively, we could have written it like this

~~~haskell
theFunctionToDraw = f

f :: R -> R -> R
f x y
  |  y>x       = 0 
  | otherwise  = 1 
~~~
@@@

<br>
**3.** One more

![](/image/lesson/images_05_houseseal.jpeg "The image produced")

This was apparently one of the old house seals in Japan. Very minimalistic. 

@@@ Show Answer
Here's a start. 

~~~haskell
theFunctionToDraw = f

f :: R -> R -> R
f x y
  | (g x y > 0.8) || (g x y < 0.2) = 1
  | otherwise   = 0

g x y = sqrt (x^2 + y^2)

~~~
Notice how we defined the distance to the origin as a separate function and re-used it. Here, `||` (the key for the symbol is near the Enter key) is the mathematical OR function, `&&` would have been AND. 
@@@

Time to get serious with the [Level 1 Problems](/Lessons/level1problems). 

<br>
<br>
