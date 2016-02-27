Functions and Types
==========================

Welcome. In this first tutorial, I will explain how to write basic mathematical functions in writing and in code. You probably intuitively know a lot of this, but I assure you, things get interesting eventually.   

### Types

Let's try to think about some kinds of mathematical objects. Here are some that I can think of:

* Integers, $$\mathbb{Z} = \{ ..,-2,-1,0,1,2,... \}$$
* Fractions, or rational numbers, denoted $\mathbb{Q}$.
* Real numbers, $\mathbb{R}$, which are numbers that represent lengths.
* Points
* Pairs (x,y) of real numbers, denoted $\mathbb{R}^2 = \mathbb{R}\times \mathbb{R}$, which correspond to points on a plane.
* More generally, pairs of any two kinds, like $\mathbb{Z}\times \mathbb{Z}$, or $\mathbb{Q}\times \mathbb{R}$, or $\mathbb{Z}\times ( \mathbb{Z} \times \mathbb{Z})$,... 
* Lines
* Circles
* ... 

Every object has a "type", which is the official word for what kind of object it is. Types help us make sure our mathematical statements make sense. If I say, "The circle $C$ is equal to the fraction $q$", it does not make sense. Types help us keep track of this kind of thing. 

 
### What is a function?

Even if you don't use the word regularly, you already know a lot about functions. 

A function is a mathematical object that takes in an input, and produces an output. 
Here is a function:
$$f(x)= x^2$$

You give $f$ a number $x$ and it gives you back, $x^2$ the square. So $f(2)=4$, $f(3)=9$, ...

Here is another function, $$h(x) = \frac{x}{2}$$. 

And another, $$g(x,y) = (y,x)$$

It doesn't have to be about numbers are coordinates though, let's write a function that takes in a circle on the plane and gives the radius
$$r(C) = \text{"radius of }C"$$ 

Functions are nice because you can model lots of things with them,... the changing of the seasons, the wings of a butterfly... But seriously, you can describe most things in the world as functions. 

### Source and Target

When we specify a function, it is important that we know what kinds of objects it takes, and what kinds of objects it doesn't. To write this down, we use the notation:
$$f: A \rightarrow B$$
which means that $f$ takes in objects of type $A$ and gives objects of type $B$ as result. $A$ is called the source and $B$ is called the target.  

For example, $f(x)=x^2$, takes in numbers and gives numbers. What kinds of numbers? It's up to us, let's say integers, denoted $\mathbb{Z}$. We write $f$ as: 

$$f:  \mathbb{Z} \rightarrow \mathbb{Z}$$
$$f(x)= x^2$$

The function $h(x) = \frac{x}{2}$ takes in numbers and gives numbers too, but what kinds of numbers? Since the answer can be $1/2$, it would be a mistake to have the target $\mathbb{Z}$. The target should be rational or real numbers. Let's just do real numbers, denoted $\mathbb{R}$. 

$$h: \mathbb{Z} \rightarrow \mathbb{R}$$
$$ h(x) = \frac{x}{2} $$

What about the function $r$ that gives the radius of a circle? It would be
$$r: \operatorname{Circles} \rightarrow \mathbb{R}$$
We could have decided to represent circles some other way, maybe by keeping track of the center and radius, but let's just have a $\operatorname{Circles}$ type for now. 

Finally, 
$$g(x,y): \mathbb{R}\times \mathbb{R} \rightarrow \mathbb{R}\times \mathbb{R}  $$
$$g(x,y) = (y,x)$$


Writing functions as code
-------------------

How do we write a function into code? If you open the 'Evaluate' page. You will see the code: 

~~~haskell
valueToPrint = f 1

f:: R -> R
f x = x
~~~

Go ahead and open a new window for your browser and open the Evaluate page. Let's press 'Run' under the code and see what happens. 

As you can see, you just write `f x` instead of $f(x)$. Also, you put two colons instead of one in the function type.

Ok, let's write our squaring function from before. 

~~~haskell
valueToPrint = f 2

f:: R -> R
f x = x^2
~~~

Go ahead and write this in and press 'Run', hopefully it outputs 4 and we are still on this planet. 

The type in Haskell for integers ($\mathbb{Z}$, that is) is called `Integer`.

~~~haskell
valueToPrint = squaremyint 2

squaremyinteger :: Integer -> Integer 
squaremtinteger n = n^2
~~~


### Pritorities

When we write,

~~~haskell
f x = x^2 + 2*x
~~~

you probably know that `^` and `*` take precendence over `+`. Exponentials also take precendence over multiplication. 
So you have to be careful about this and use parantheses when necessary. For example, if we really wanted $(x^2+2)x$, you would write,

~~~haskell
f x = (x^2 + 2)*x
~~~

just like in math. 
You can use as many parantheses as you like, as long as they 'match' and you don't have stray parantheses flying around. 

~~~haskell
f x = (((x^2 + 2))*x)
-- noooo problem. by the way, this is how you write comments in your code
~~~

Let's open a new Graph page and run the following code. 

~~~haskell
-- this is the function that will be graphed 
theFunctionToGraph = f
 
f:: R -> R
f x = sin 2*x 
~~~

What happened? Yes, `sin 2*x` is understood by the language as `(sin 2)*x`, function application has precedence over other operations. So if you really wanted $\operatorname{sin}(2x)$, you would need to write

~~~haskell
f x = sin (2*x) 
~~~

**Exercise** What will the following code output?

~~~haskell
valueToPrint = f 2

f:: R -> R
f x = g x+1*2

g:: R -> R
g x = x + x * x 
~~~
Click 'Show' to see the answer. 
@@@ Show
We know that function application is first, then multiplication, then addition

~~~haskell 
valueToPrint = f 2

f:: R -> R
f x = (g x)+(1^2)

g:: R -> R
g x = x + (x * x) 
~~~
So `g 2 = 2 + (2*2) = 6`
So `f 2 = (g 2) + 1 = 7`
@@@



Graphing functions
-------------------

If you now open the 'Graph' page, you can draw the graph of any function $f:R -> R$ that you like. 
Yes, our grandparents were doing this on their calculators when they were in primary school, but at least we are doing it in style.

Let's try running the following. (go ahead and paste this code in the Graph page and run it)
 
~~~haskell
-- this is the function that will be graphed 
theFunctionToGraph = f
 
f:: R -> R
f x = -(x^2)+1
~~~

Ok, based on the graph that appears below the run button after running, we should be able to figure out what this grapher is doing. 

**Question**: Which one's the $x$ axis, which one's the $y$-axis? What $x$ and $y$ values correspond to the left, right, top and bottom ends of the picture.
@@@ Show Answer
Looking at $f$, we see that $f(0) = 1$, $f(1)=0=f(-1)$. So we can see that the right end-point is $(1,0)$, left end-point is $(-1,0)$, the top end-point is $(0,1)$. So this grapher is boxed in with $-1\leq x \leq 1$ and  $-1\leq y \leq 1$.
@@@

**Exercise**: 
Here's a funny graph. 

![](/image/lesson/graph_question_1.jpeg "The graph in question")

Let's figure out which function is going to give us the graph above. Once you think you got it, go ahead and write it into the Graph page to see if you got it right. (Hint, you will need the number $\pi$, in Haskell, it is called `pi`) Click 'Show' below to see the answer. 
@@@ Show Answer
The function looks like the sine function. But if we just graph `f x = sin x`, it doesn't look the same because it doesn't go up and down twice and end at $0$, that's why we put 

~~~haskell
theFunctionToGraph = f
 
f:: R -> R
f x = sin (2*pi*x)
~~~

@@@

### Some built-in functions

Here are some standard functions in Haskell and their types. 
 
~~~haskell
floor :: R -> Integer
-- "floor x" is  the greatest integer not greater than x 

abs :: R -> R
-- absolute value
 
log :: R -> R
-- logarithm function
~~~


## Defining functions case by case

Let's say I want to define the function which has the following graph:

![](/image/lesson/graph_question_2.jpeg "The graph in question")

I would like to say that if $x<0$ then $f(x) = -0.5$ and if  $x\geq 0$ then $f(x) = 0.5$. Here is how you write this in code:

~~~haskell
theFunctionToGraph = f
 
f:: R -> R
f x = if x<0 then 0.5 else (-0.5) 
~~~

Now what will this code produce?

~~~haskell
theFunctionToGraph = f
 
f:: R -> R
f x = if (x^2<0.49) then x^2 else 0.49
~~~
	
<br>
How would you make a function with this graph?

![](/image/lesson/graph_question_3.jpeg "The graph in question")


Here's how you would write this function in math:

$$f(x) =
\left\{
	\begin{array}{ll}
		0  & \mbox{if } x \leq 0 \\
		x & \mbox{if } 0 < x \leq 0.5 \\
                0.5 & \mbox{otherwise }
	\end{array}
\right.$$

**Exercise** Let's write the code for this function using the `if-then-else` combo.

@@@ Show Answer

We want to make $f(x)=0$ when $x<0$, $f(x)=x$ when $0<x<0.5$ and $f(x)=0.5$ for $x>0.5$.

~~~haskell
theFunctionToGraph = f
 
f:: R -> R
f x = if x<=0 then 0 else (if x<=0.5 then x else 0.5)
~~~
@@@

There is actually another way to do this, which looks more like the math.

~~~haskell
f:: R -> R
f x 
  | x<=0       = 0
  | x<=0.5     = x
  | otherwise  = 0.5 
~~~

Notice how there is no equals sign after `f x`. The way this works is that the computer checks the cases line by line. In the first line, if `x<=0` then it's done, the answer is 0. It only looks at the second line if it is not true that `x<=0`. Finally, if it gets to the `otherwise`, then it just says what it says there, i.e. give `0.5`.

<br><br> 



