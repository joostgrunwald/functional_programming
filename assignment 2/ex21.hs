
# Ex 2.1
# Create a Haskell file with the function definition

swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)

dupl :: (Int, Int) -> (Int, Int)

dupl (a, b) = (a * 2, b * 2)

incr :: (Int, Int) -> (Int, Int)
incr (a, b) = (a + 1, b + 1)

# Ex 2.2
# The function no longer is restricted to inputs of type Integer.
# It would now work fine aswell with for example strings, floats, etc.
# for the other functions this is more problematig, adding 1 to string probably
# Will lead to errors, multiplying a string by 2 will probably lead to a double string

# Ex 2.3
# swap :: (a, b) -> (b, a)

# Ex 2.4
# (Int,(Char,Bool)) is a tuple of two elements consiting of an int and another tuple with two elements
# (Int,Char,Bool) is a tuple of three elements consisting of an int, char and bool
# Hence for the first type x[1][0] would be Char and for the second x[1] would be Char and x[1][0] would be invalid

conv :: (a,(b,c)) -> (a, b, c)
conv (a, (b, c)) = (a, b, c)

conv2 :: (a, b, c) -> (a, (b, c))
conv2 (a, b, c) = (a, (b, c))