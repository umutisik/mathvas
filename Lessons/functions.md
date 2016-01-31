Writing down and evaluating functions
============================================================

The coding environment of Mathvas is a place where you can write functions. Here is a function:

$$f:  \mathbb{R} \rightarrow \mathbb{R}$$
$$f(x)= x^2$$

This function takes in a number, and returns the square. 

Here is how you would write it as code:

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



