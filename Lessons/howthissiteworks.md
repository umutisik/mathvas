Functions and this site
==========================

Welcome. In this first tutorial, I will explain how to write basic mathematical functions in Haskell and how to use this web site. Feel free to read the parts you may already know about more quickly; but I assure you things get interesting eventually.   

What is a function?
-------------------

Even if you don't use the word regularly, you already know a lot about functions. 

A function is a mathematical object that takes in an input, and gives an output. 
Here is a function:
$$f(x)= x^2$$

You give $f$ a number $x$ and it gives you back, $x^2$ the square. So $f(2)=4$, $f(3)=9$ etc.

When writing down a function, we can also write what kind things it takes and what kinds of things it gives. What does $f(x)=x^2$ take and give? It takes in numbers and gives numbers. What kinds of numbers? Let's say real numbers. If we write the set of real numbers as $\mathbb{R}$, then we can write $f$ as: 

$$f:  \mathbb{R} \rightarrow \mathbb{R}$$
$$f(x)= x^2$$.

When you write $f: A \rightarrow B$, you call $A$ the source and $B$ the target.

**Question**: What's the nicest function that you can think of?

Here is another function:
$$g(x,y): \mathbb{R}\times \mathbb{R} \rightarrow \mathbb{R}\times \mathbb{R}  $$
$$g(x,y) = (y,x)$$

Functions are nice because you can model lots of things with them,... the changing of the seasons, the wings of a butterfly... But seriously, you can describe most things in the world as functions. What does $g$ above do?


Writing functions as code
-------------------

How do we write a function into code? If you open the 'Evaluate' page. You will see the code: 

~~~haskell
valueToPrint = f 1

f:: R -> R
f x = x
~~~

Go ahead and open a new window for your browser and open the Evaluate page. Let's press 'Run' under the code and see what happens. 

As you can see, you just write `f x` instead of $f(x)$. Also, you put two colons instead of one.

Ok, let's write our squaring function from before. 

~~~haskell
valueToPrint = f 2

f:: R -> R
f x = x^2
~~~

Go ahead and write this in and press 'Run', so we can see that we are still on this planet. 

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



Graphing functions
-------------------

If you now open the 'Graph' page, you can draw the graph of any function you like. 
Yes, our grandparents were doing this on their calculators when they were in primary school, but at least we are doing it (kind of) in style.

Let's try running the following. 
 
~~~haskell
-- this is the function that will be graphed 
theFunctionToGraph = f
 
f:: R -> R
f x = -(x^2)+1
~~~

Ok, based on the graph that appears below the run button after running, we should be able to figure out what this grapher is doing. 

**Question**: Which one's the $x$ axis, which one's the $y$-axis? What $x$ and $y$ values correspond to the left, right, top and bottom ends of the picture.
@@@
Looking at $f$, we see that $f(0) = 1$, $f(1)=0=f(-1)$. So we can see that the right end-point is $(1,0)$, left end-point is $(-1,0)$, the top end-point is $(0,1)$. So this grapher is boxed in with $-1\leq x \leq 1$ and  $-1\leq y \leq 1$.
@@@

**Exercise**: 
Here's a funny graph. 

![](/static/img/graph_question_1.jpeg "The graph in question")

Let's figure out which function is going to give us the graph above. Use the grapher to see if you got it right before looking. 
@@@
The function looks like the sine function. But if we just graph `f x = sin x`, it doesn't look the same because it doesn't go up and down twice and end at $0$, that's why we put 

~~~haskell
theFunctionToGraph = f
 
f:: R -> R
f x = sin (2*pi*x)
~~~
@@@




asdf
-------------


This is a very simple site. There are two types of pages: tutorials and activities. 

On the activities pages, there is a coding environment where you can:

* Evaluate functions
* Graph functions
* Make Grayscale images from functions
* Make color Images from functions
* Write and run haskell programs

That's right, all the coding on this site is based on the programming language called Haskell.



$$f:  \mathbb{R} \rightarrow \mathbb{R}$$
$$f(x)= x^2$$

This function takes in a number, and returns the square. 

Here is how you would write it as code:
* Mathematical functions that are evaluated (Evaluate).

~~~haskell
f :: R -> R
f x = x^2
~~~

This is a good time to open a new window, go to the 'New' page and click 'Evaluate'. 

@@@
This is the answer bruh.
Note: It is not clear what the question was.
@@@


@@@
This is the answer bruh.
Note: It is not clear what the question was.
@@@


@@@
This is the answer bruh.
Note: It is not clear what the question was.
@@@


@@@
This is the answer bruh.
Note: It is not clear what the question was.
@@@

#An h1 header

Paragraphs are separated by a blank line.

2nd paragraph. *Italic*, **bold**, and `monospace`. Itemized lists
look like:

  * this one
  * that one
  * the other one

Note that --- not considering the asterisk --- the actual text
content starts at 4-columns in.

> Block quotes are
> written like so.
>
> They can span multiple paragraphs,
> if you like.

Use 3 dashes for an em-dash. Use 2 dashes for ranges (ex., "it's all
in chapters 12--14"). Three dots ... will be converted to an ellipsis.
Unicode is supported. ☺



An h2 header
------------

Here's a numbered list:

 1. first item
 2. second item
 3. third item

Note again how the actual text starts at 4 columns in (4 characters
from the left side). Here's a code sample:

    # Let me re-iterate ...
    for i in 1 .. 10 { do-something(i) }

As you probably guessed, indented 4 spaces. By the way, instead of
indenting the block, you can use delimited blocks, if you like:

~~~
define foobar() {
    print "Welcome to flavor country!";
}
~~~

(which makes copying & pasting easier). You can optionally mark the
delimited block for Pandoc to syntax highlight it:

~~~haskell
import Time
testo f = case f of 
            _ -> 1

-- this is the function that will be drawn 
theFunctionToDraw = f

f :: R -> R -> R
f x y = weir (x^2+y^2)

weir t 
    | t<=0       = 0
    | t>=1       = 1
    | otherwise  = (normasin (100*t))*(sqrt $ sqrt t)
    
normasin t = (1 + sin t)/2


~~~

@@@
This is the answer bruh.
Note: It is not clear what the question was.
@@@

@@@
This is the answer bruh.
Note: It is not cl asldf kalsj dfkl;ja slkdfjal;ksj dflkj as;ldkfj l;kaj sdl;kfj as;lkdjf ;lkasj dfl;kj asl;kdjf l;kasj dfl;kj asl;kdfj l;kasj dfl;kj asl;kdj fl;kajs dfl;kjasl;dkfj al;ksjdfl;kajs dl ;fkja;lksjd f;lkajs d;lkfj al;ksdj f;klajsd f;lkjas;dklfjakl;s jdf;klaj sd;lfkj a;klsdjf lkajsdf ;lkajsdlk f



~~~haskell
import Time
testo f = case f of 
            _ -> 1

-- this is the function that will be drawn 
theFunctionToDraw = f

f :: R -> R -> R
f x y = weir (x^2+y^2)

weir t 
    | t<=0       = 0
    | t>=1       = 1
    | otherwise  = (normasin (100*t))*(sqrt $ sqrt t)
    
normasin t = (1 + sin t)/2


~~~




ear what the question was.
@@@


@@@
This is the answer bruh.
Note: It is not clear what the question was.
@@@




### An h3 header ###

Now a nested list:

 1. First, get these ingredients:

      * carrots
      * celery
      * lentils

 2. Boil some water.

 3. Dump everything in the pot and follow
    this algorithm:

        find wooden spoon
        uncover pot
        stir
        cover pot
        balance wooden spoon precariously on pot handle
        wait 10 minutes
        goto first step (or shut off burner when done)

    Do not bump wooden spoon or it will fall.

Notice again how text always lines up on 4-space indents (including
that last line which continues item 3 above).

Here's a link to [a website](http://foo.bar), to a [local
doc](local-doc.html), and to a [section heading in the current
doc](#an-h2-header). Here's a footnote [^1].

[^1]: Footnote text goes here.

Tables can look like this:

size  material      color
----  ------------  ------------
9     leather       brown
10    hemp canvas   natural
11    glass         transparent

Table: Shoes, their sizes, and what they're made of

(The above is the caption for the table.) Pandoc also supports
multi-line tables:

--------  -----------------------
keyword   text
--------  -----------------------
red       Sunsets, apples, and
          other red or reddish
          things.

green     Leaves, grass, frogs
          and other things it's
          not easy being.
--------  -----------------------

A horizontal rule follows.

***

Here's a definition list:

apples
  : Good for making applesauce.
oranges
  : Citrus!
tomatoes
  : There's no "e" in tomatoe.

Again, text is indented 4 spaces. (Put a blank line between each
term/definition pair to spread things out more.)

Here's a "line block":

| Line one
|   Line too
| Line tree

and images can be specified like so:

![example image](/static/img/empty-image.jpg "An exemplary image")

Inline math equations go in like so: $\omega = d\phi / dt$. Display
math should get its own line and be put in in double-dollarsigns:

$$I = \int \rho R^{2} dV$$

And note that you can backslash-escape any punctuation characters
which you wish to be displayed literally, ex.: \`foo\`, \*bar\*, etc.



