Level 3 Problems
============================================================

These problems rely on making functions that make new images from your previously-made images. This allows you to make more complicated images from simple ones. 

**Repeat Four Times** 

![](/image/lesson/level3_repeat.jpeg "Problem Image")

It doesn't matter which picture you repeat four times. 

@@@ Show hint
Your code could look like:

~~~haskell
theFunctionToDraw = repeat f

repeat :: (R->R->R) -> R -> R -> (R,R,R)
repeat f x y 
  | x<0 && y<0   = f (...) (...)
  | x<0 && y>=0  = f (...) (...)
  | x>=0 && y<0  = f (...) (...)
  | otherwise    = f (...) (...)

~~~
@@@
<br>

**Popartish**

![](/image/lesson/level3_popart.jpeg "Problem Image")

@@@ Show hint
Your code should look like:

~~~haskell
theFunctionToDraw = popart f

popart :: (R->R->R) -> R -> R -> (R,R,R)
popart f x y ...

f = ...
~~~
@@@
<br>


**Popart, More Colorry**

![](/image/lesson/level3_popart2.jpeg "Problem Image")

<br>

The following problems are harder. Remember that you don't *have to* do problems. If you have lots of ideas about what to make, then just go and make them and come back here when you need techniques/inspiration. 



**Spiral** 

![](/image/lesson/level3_spiralsm.jpeg "Problem Image")

This is a hard one.

@@@ Show hint
Make a function called `spiralify`. It should take a function, and rotate it more the farther you are from the origin.
@@@
<br>



**Repeat And Rotate** 

![](/image/lesson/level3_repeatsquaresm.jpeg "Problem Image")

Another good trick to make complex pictures from simple pictures. 

@@@ Show hint
First make a function 

~~~haskell
nrepeat :: Int -> (R -> R -> R) -> R -> R -> R
~~~
which will repeat the same picture `n` times in an `n` by `n` grid. Then make another version of nrepeat, which will actually change the picture it is repeating. 
@@@
<br>





**n-fold Symmetry** 

![](/image/lesson/level3_3foldsym.jpeg "Problem Image")

This, and the next few problems realy on taking a picture, taking one slice, and repeating it a number of times. Try to do everything so that the number of slices, and therefore the angle of symmetry come from a variable.

There is a function called `atan2 :: R -> R -> R` which is already in the standard set of functions. The value `atan2 y x` will give you the angle between the $x$ axis the line from the origin to the point $(x,y)$. 

@@@ Show hint

Your code could look like

~~~haskell
theFunctionToDraw = nfoldsym f

nfoldsym :: Int -> (R-> R -> R) -> (R->R->r)
nfoldsym n f x y = ...
           where angle = atan2 y x
                 dis = sqrt (x^2 + y^2)
~~~
@@@

<br>


**ngon** 

![](/image/lesson/level3_pentagon.jpeg "Problem Image") 

![](/image/lesson/level3_heptagon.jpeg "Problem Image")

`theFunctionToDraw = ngon 5` should draw the 5-gon. `ngon 7` should draw the 7-gon.

<br>

**More 5-fold symmetry** 

![](/image/lesson/level3_pentagonshade.jpeg "Problem Image") 

![](/image/lesson/level3_cool5symsm.jpeg "Problem Image")

<br>



<!--Time to get serious with [Color](/Lessons/colorimages). -->

<br>
<br>
