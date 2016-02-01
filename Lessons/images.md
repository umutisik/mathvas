Making Images
============================================================

(under construction)

To make images, select either 'Grayscale' or 'Images' in the 'New' menu.

For grayscale, the function to be drawn has type `f:: R -> R -> R`. 
For example:

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = f

f :: R -> R -> R
f x y = x^2+y^2 

~~~

`f` takes in two parameters, these are the first two `R`'s there. The number produced corresponds to the color of the pixel at the $(x,y)$ coordinates. Values `0` or smaller are is black, and `1` or larger makes the pixel white. Values in between are shades of gray. 

What will the code above produce? 

...

