Important information:
I have changed the command "ILLITERAL" a bit to restore reversibility.
Consider this program (using the default Snowflake language):
```
[-1-1+1]
```
The second ILLITERAL would pop the -0 pushed by the first, leaving an empty stack for the LITERAL, which then pushes a +0.
This is done because ILLITERAL doesn't check for polarity.
Quote from the specification:
"... or including with the same polarity (for ILLITERAL), it is popped."
But the effects of -1 and +1 should cancel eachother out, leaving the -0 pushed by the first ILLITERAL on the stack!
