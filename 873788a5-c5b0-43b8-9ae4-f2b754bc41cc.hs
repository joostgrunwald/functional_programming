module Database where

type Person = (Name, Age, FavouriteCourse, Courses, SocialSecurityNr, Department)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String
type Courses          = [String]
type SocialSecurityNr = Integer
type Department       = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming",  ["FP", "AI", "Logic"], 123456789,  "CS")
peter  =  ("Peter",  57,  "Imperative Programming", ["IP", "SEC", "Logic"], 987654321,  "CS")
pol    =  ("Pol",    36,  "Object Oriented Programming", ["OOP", "MAT", "Logic"], 123456789,  "CS")
polstwin   =  ("Pol2",    36,  "Object Oriented Programming", ["OOP", "MAT", "Logic"], 123456789,  "CS")
frits   =  ("Frits",    23,  "Object Oriented Programming", ["OOP", "MAT", "Logic"], 123456789,  "CS")

students :: [Person]
students = [elena, peter, pol, frits]

age :: Person -> Age
age (_, a, _, _, _, _)  =  a

name :: Person -> Name
name (n, _, _, _, _, _)  =  n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, c, _, _, _)  =  c

showPerson       :: Person -> String
showPerson (n, a, c, cs, ssn, d)  =  n ++ " is " ++ show a ++ " years old and likes " ++ c ++ " and is teaching " ++ show cs ++ " and has ssn " ++ show ssn ++ " and is in department " ++ d

twins            :: Person -> Person -> Bool
twins(_, a1, _, _, _, _) (_, a2, _, _, _, _)  =  a1 == a2

increaseAge      :: Person -> Person
increaseAge (n, a, c, cs, ssn, d)  =  (n, a+1, c, cs, ssn, d)

query1 :: [Person]
query1 = map (increaseAge) (map (increaseAge) students)

--helper function
promotestudent :: Person -> Person
promotestudent (n, a, c, cs, ssn, d)  =  ("dr. " ++ n, a, c, cs, ssn, d)

query2 :: [Person]
query2 = map (promotestudent) students

--helper function
isFrits :: Person -> Bool
isFrits (n, _, _, _, _, _)  =  n == "Frits"

query3 :: [Person]
query3 = filter (isFrits) students

--helper function
inTwenties :: Person -> Bool
inTwenties (_, a, _, _, _, _)  =  a >= 20 && a < 30

query4 :: [Person]
query4 = filter (inTwenties) students

--helper function
getAge :: Person -> Integer
getAge (_, a, _, _, _, _)  =  a

--query5 :: [Person] -> Age
query5 = sum(map (getAge) students)

query6 :: [Person]
query6 = _TODO "promote the students whose favourite course is Functional Programming"

-- if you have removed all TODO's, remove these lines
_TODO :: String -> a
_TODO msg = error ("TODO: " ++ msg)
