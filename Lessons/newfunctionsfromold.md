Functions from other functions
============================================================

So far, if we have a picture in mind, we know how to go about writing a function `f::R->R->R` if it is a black and white function, or a function `f:: R->R->(R,R,R)` if it is a function with colors.

What if I've already made a function. Like

~~~haskell
f x y =  sin (x^3 + y^3)
~~~ 

It draws:

![](/image/lesson/newfromold_example1b.jpeg "The image produced")

This looks like it should be a complicated function. `sin` should oscillate. But we are seeing a very simple picture. What is going on?  
Maybe we are missing out on a lot of values because, half the time the sine function is below zero. That's why there is so much black in this picture. Maybe the complicated parts are in the black part? To make up for that, we can replace `sin z` by `(1+(sin z))/2`. This would make the function osciallate between 0 and 1.  
 
~~~haskell
f x y =  norma (sin (x^3 + y^3))
norma t = (t+1)/2
~~~ 

A good way to prevent our code from getting complicated is to make new functions for simple tasks. so I added a new function called `norma` here to do that. 

This will draw:

![](/image/lesson/newfromold_example2b.jpeg "The image produced")

Ok, this got rid of the black portion, but it still isn't a very complicated image. We should zoom out to see what happens farther away from the origin. After all, we are just seeing the parts where the coordinates are between -1 and 1 here. 

But do we zoom out? If you've done the previous exercises, you would have noticed that you can zoom out by multiplying `x` and `y` by a number. But I don't want to do that every time I want to adjust the zoom level. So, instead of doing that, I am going to make a function, which turns any function to its zoomed version. 

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = zoomout f

zoomout :: (R -> R -> R) -> (R -> R -> R) 
zoomout f x y = f (3*x) (3*y)

f x y =  norma $ sin (x^3 + y^3)
norma x = (x+1)/2
~~~

The key part of the code is the function `zoom`. According to its type, it takes a number `amount` and a function `f`, which is of type `(R->R->R)`. From this, it gives a function called `(zoomout f)` of type `(R->R->R)`. When this function is drawn. That's right, you can have functions that map functions to other functions. This one turns a function `f` into a function `zoomout f` where `(zoomout f)` is `f` evaluated at points three times as far from the origin. That's why we wrote  `zoomout f x y = f (3*x) (3*y)`. 

![](/image/lesson/newfromold_example3.jpeg "The image produced")

We got the zoomed out image, which has the osciallations as expected. We could actually write a function that zoom an arbirary amount

~~~haskell
zoom:: R -> (R -> R -> R) -> (R -> R -> R) 
zoom amnt f x y = f (x/amnt) (y/amnt)
~~~

The previous `zoomout f` would be `zoom 0.333 f`. 

####Moving the picture
Similar to zoom, we could make a function that moves the picture in the directiont that we want. Try to make a function of type

~~~haskell
move :: R -> R -> (R->R->R) -> (R->R->R)
move mx my f x y = ... 
~~~
that would move the picture by the `mx` amount to the right and the `my` amount upwards. So for example 

~~~haskell
theFunctionToDraw = move 0.9 (-0.3) (zoomout f)
~~~ 
Would produce: 

![](/image/lesson/newfromold_example4.jpeg "The image produced")

Very dramatic! Try to write this move function. 

@@@ Show answer

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = move 0.9 (-0.3) (zoomout f)

move :: R -> R -> (R->R->R) -> (R->R->R)
move mx my f x y = f (x-mx) (y-my)

zoomout :: (R -> R -> R) -> (R -> R -> R) 
zoomout f x y = f (3*x) (3*y)

f x y =  norma $ sin (x^3 + y^3)
norma x = (x+1)/2
~~~
@@@


#### Rotating Images

This one is nice. For this one, you need a little bit of trigonometry. Let's say I have drawn a nice square like this:

~~~ haskell
theFunctionToDraw = squa

squa:: R -> R -> R
squa x y = if ((abs x < 0.5) && (abs y) < 0.5) then 1 else 0
~~~

![](/image/lesson/newfromold_square.jpeg "The image produced")

Very minimal!

Now I want to rotate this square a little bit to the left. 

If I rotate the plane by an angle theta, where would the point $(x,y)$ end up? I can solve this problem using trigonometry. Here is the answer: $(x,y)$ it ends up at
$$(x\cos{\theta}-y\sin{\theta}, x\sin{\theta}+y\cos{\theta})$$
So, to rotate the function `f`, we just evaluate `f` at the rotated point (by minus $\theta$ so that the picture rotates to the left).

~~~haskell
theFunctionToDraw = rotate (pi/12) squa

squa:: R -> R -> R
squa x y = if ((abs x < 0.5) && (abs y) < 0.5) then 1 else 0

rotate :: R -> (R->R->R) -> (R->R->R)
rotate theta f x y = f (x*ct - y*st) (x*st + y*ct) 
                                      where ct = cos (-theta)
                                            st = sin (-theta)
~~~

![](/image/lesson/newfromold_rotatedsquare.jpeg "The image produced")


####Messing with color

Let's make the following function. It takes any black and white picture, and turns it into a four-color picture. It should take a function `(R->R->R)` which would make a grayscale function, and it should produce a function of type `R -> R -> (R,R,R)`, which is the kind of function that makes a color picture. So, it should have type: 

~~~haskell
colorify :: (R -> R -> R) -> (R -> R -> (R,R,R))
~~~

Here is the example code. The function `g` here is like the osciallating function above, but it looks nicer with squares. 

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = colorify g

colorify :: (R->R->R) -> (R->R->(R,R,R))
colorify f x y 
  | x>=0 && y>=0    = (f x y, 0,0)
  | x>=0 && y<=0    = (0,f x y, 0)
  | x<=0 && y>=0    = (0,0,f x y)
  | otherwise       = (f x y, f x y, 0)

g = zoom 0.333 f
f x y =  norma $ sin (x^2 + y^2)
norma x = (x+1)/2

zoom:: R -> (R -> R -> R) -> (R -> R -> R) 
zoom amnt f x y = f (x/amnt) (y/amnt)
~~~

And here is the outcome. 

![](/image/lesson/newfromold_color1.jpeg "The image produced")

#### Another colorify

Maybe we can get something better if we make a 'colorify' that takes a black and white image, and will just make each shade of gray into some weird color. 

We can make a function `weirdColorSelect :: R -> (R,R,R)` that sends a value to some weirdly chosen color.  And then set: 
`colorify2 f x y = weirdColorSelect (f x y)`

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = colorify2 g

colorify2 :: (R->R->R) -> (R->R->(R,R,R))
colorify2 f x y = weirdColorSelect (f x y)

weirdColorSelect :: R -> (R,R,R)
weirdColorSelect r = (norsin (10*r), norsin (10*r^2), norsin (10*r^3 ))

g = zoom 0.333 f
f x y =  norsin (x^2 + y^2)
norma x = (x+1)/2
norsin x = norma (sin x)

zoom:: R -> (R -> R -> R) -> (R -> R -> R) 
zoom amnt f x y = f (x/amnt) (y/amnt)
~~~

![](/image/lesson/newfromold_color3.jpeg "The image produced")

Trippy!!! Of course the nice thing about making a function from functions to functions is that we can use if for any function!

~~~haskell
-- this is the function that will be drawn 
theFunctionToDraw = colorify2 g

g x y = abs x + abs y

colorify2 :: (R->R->R) -> (R->R->(R,R,R))
colorify2 f x y = weirdColorSelect (f x y)

weirdColorSelect :: R -> (R,R,R)
weirdColorSelect r = (norsin (10*r), norsin (10*r^2), norsin (10*r^3 ))
~~~

![](/image/lesson/newfromold_color4.jpeg "The image produced")

<br>
<!--Time to get serious with the [Level 2 Problems](/Lessons/level2problems). -->

<br>
<br>
