--addOne :: Int -> Int
addOne x = x + 1
----Cant call this on any type (strings, arrays etc.)

addOne :: Num a => a -> a

addOne' = (+1) -- curried: defined in terms of the function +1 rather than x


addOneToAll :: Num b => [b] -> [b]
addOneToAll xs = map (\x -> x+1) xs -- maps the lambda over a list where x is the list, the parentheses is the lambda
									-- applies the function to each element in the list one by one

addOneToAll' :: Num b => [b] -> [b]
addOneToAll' = map (+1)

--Haskell doesn't have a null type, has a maybe type. Other languages have copied this (Java, Scala, C#, JS, Ruby)

----Maybe (null in Haskell)

data Maybe a = Nothing | Just a
	deriving(Eq, Show)  -- takes in a type class (eq and show) and generates the equal function and the show function

inverse 0 = Nothing
inverse x = (1/x)
--Calling two instances of inverse which are not the same type will give errors.



data Mebe a = Nada | Gotta a
	deriving (Eq, Show)

inverse 0 = Nada
inverse x = Gotta (1/x)

safeHead :: [a] -> Mebe a
safeHead [] = Nada
safeHead (x:xs) = Gotta x -- pulling off the head into one var and putting the rest into another

find :: (a -> Bool) -> [a] -> Mebe a
find f [] = Nada
find f (x:xs) = if f x
				then Gotta x
				else find f xs

instance Functor Mebe where
	fmap f Nada = Nada
	fmap f (Gotta x) = Gotta (f x)



______________________________
CONSOLE
______________________________

addOne 2
>> 3

addOne 2.5
>> error

:t inverse 2
>> Eq a, Fractional s => Maybe a

:t inverse (2::Double)
>> 