-- In the program, the Глушков's algorithm has been used
-- to transform the regular expression to an NFA

-- I assume the only possible oerators are: + * ( ) 

import Debug.Trace

data RegularExpression = Character | NullCharacter | Star {argument :: RegularExpression} | Plus {first_argument :: RegularExpression, second_argument :: RegularExpression} | Composition {arguments :: [RegularExpression]} | Unparsed {unparsed :: String} deriving Show

_sub_sums :: Num a => [a] -> a -> [a]
_sub_sums (x:xs) old_sum = new_sum : (_sub_sums xs new_sum)
				where new_sum = x + old_sum
_sub_sums [] old_sum = []

sub_sums :: Num a => [a] -> [a]
sub_sums array = _sub_sums array 0

substring :: String -> Int -> Int -> String
substring string beginning end = drop beginning $ take end string

parse_brackets :: String -> [RegularExpression]
parse_brackets to_parse | unpaired_opening_bracket_encountered = error "Unpaired opening bracket found"
			| unpaired_closing_bracket_encountered = error "Unpaired closing bracket found"
			| otherwise = regexes
	where
		bracket_levels = sub_sums $ map (\x -> if x == '(' then -1 else if x == ')' then 1 else 0) to_parse
		unpaired_closing_bracket_encountered = foldl (\x y -> x || y > 0) False bracket_levels
		first_level_bracket_positions = filter (\x ->   
								((bracket_levels !! x == -1) && (to_parse !! x == '(')) 
									||
								((bracket_levels !! x == 0) && (to_parse !! x == ')'))
							)  
							[0 .. (length to_parse - 1)]
		unpaired_opening_bracket_encountered = (last bracket_levels) < 0
		all_points = (if (head first_level_bracket_positions == 0) then [] else [0]) ++ first_level_bracket_positions ++ (if (last first_level_bracket_positions == (length to_parse - 1)) then [] else [length to_parse - 1])
		split_points = map (\x -> if (to_parse !! x == ')' || x == length to_parse -1) then x + 1 else x) all_points
		split_strings = map (\x -> substring to_parse (split_points !! x) (split_points !! (x + 1))) [0 .. length split_points - 2]
		regexes = map(\x ->
				if head x == '(' then
					parse $ substring x 1 (length x - 1)
				else 
					Unparsed x
			) split_strings

parse_characters :: String -> [RegularExpression]
parse_characters to_parse = [NullCharacter]
	where
		positions_a = map (\x -> if (x == 'a') then True else False) to_parse




parse :: String -> RegularExpression
parse string_to_parse = Composition $ parse_brackets string_to_parse -- debug only



-- main :: IO ()
-- main = putStrLn "Yes"
