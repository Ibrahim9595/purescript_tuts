# Fold by other names

In many non-Functional Languages, fold is also called reduce. In the case of our length function, we
reduce our List to a single Int. But this is NOT always the case. We can use fold to break down one
structure and build up another, e.g. process a List and generate a Map (a Key/Value store).


And in Category Theory, you’ll hear the term Catamorphism (Cata means down and morph means
shape), essentially meaning to break down structure.
This is opposed to Anamorphism (Ana means up and morph means shape), essentially meaning to build
up structure.


If you forget these terms, you can remember Anabolic Steroids which builds muscle or Catabolic Exercise
which breaks down muscle tissue and burns fat.


The point here is that folds are called Catamorphisms because they break down the original structure.
There’s also another concept called unfold, which are used far less often, and are called Anamorphisms
since they build up structure from a single start value.
We are going to concern ourselves with just folds here though.