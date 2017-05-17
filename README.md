# BCBL
This is mostly a joke. I had to create business cards and was given by the university a form that restricted the layout and number of characters. I decided to have fun and wrote on my cards a little implementation of propositional logic in the SK calculus, a sort of business card boolean logic (BCBL) if you will. Then I decided to see if I could actually implement it in Haskell. It was fun!

## Are there any good ideas in here?

Yes! It is nice to see how reduce expressions of the SK calculus via pattern matching in Haskell. It is so elegant. Doing this with a stack or something seems hellish in comparison.

Also, the definition of the sentential connectives is pretty cute. Note that NOT is a post-fix operator while OR is an "infix" operator with asymmetric application of the disjuncts--i.e.,

* NOT x := x (f) (t) 
* x Or y := x (t) (y)
