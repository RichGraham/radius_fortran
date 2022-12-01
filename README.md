# Handling a conflict in real code
The example involves some real code. The aim of the code is, given the
area of a circle, A, evaluate the radius, r, via the formula:

$r=\sqrt{A/\pi}$,
	
and also to format the output.

## Understanding the three branches 

### The branch 'main'
The main branch represents an initial attempt to implement this. Take
a look at the code in the file radius.py and make sure you can run
this code. You'll likely find several issues with this code and
perhaps have some ideas for how to improve this.

Resist the urge to
make changes for now. Imagine instead that two co-workers have
separately and
simultaneously worked on this code in the branches 'better-powers' and
'proper-sqrt'. **Our task is to understand these changes and merge these
to take the best changes from each branch**.


### The branch 'better-powers'
This is a branch from master and it attempts to improve the method for
evaluating the powers of $x$ in the calculate_radius function and some
other minor fixes. Compare this branch and main and try understand
what changes have been made and why. Run the code for this branch and
check it gives the output you expect.

### The branch 'proper-sqrt'
This is a also branch from main and it replaces the Taylor series in
the calculate_radius function with a built-in square-root function. It
also makes some other minor fixes. Again, compare this branch and main and try understand
what changes have been made and why. Run the code for this branch and
check it gives the output you expect.

## Merging the changes
Attempt to merge the changes in 'better-powers' and 'proper-sqrt' into
a single branch. This will produce a conflict, which you'll need to
resolve. Resolve the conflict so that the merged version uses the
built in square-root function and value of $\pi$, avoids spelling errors
in the output and includes units in the output.

Once you've resolved the merge and completed the conflict, make sure
that the final code runs and outputs what you expect.



