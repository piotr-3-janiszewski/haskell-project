-- In the program, the Глушков's algorithm has been used
-- to transform the regular expression to an NFA

-- I assume the only possible oerators are: + * ( ) 

--------------------Parsing-----------------------

import System.Environment (getArgs)
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

split_at :: String -> [Int] -> [String]
split_at string [] = split_at string [0, length string]
split_at string split_points = map (\x -> substring string (all_split_points !! x) (all_split_points !! (x + 1))) [0 ..  length all_split_points - 2]
				where all_split_points = (if head split_points /= 0 then [0] else []) ++ split_points ++ (if last split_points /= (length string) then [length string] else [])

parse_brackets :: RegularExpression -> [RegularExpression]
parse_brackets (Unparsed to_parse) | unpaired_opening_bracket_encountered = error "Unpaired opening bracket found"
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
		split_points = map (\x -> if (to_parse !! x == ')' || x == length to_parse -1) then x + 1 else x) first_level_bracket_positions
		split_strings = filter (\x -> if x /= "" then True else False) $ split_at to_parse split_points
--		regexes = trace (show split_strings)  map(\x ->
		regexes = map(\x ->
				case x of
				('(':_:xs) -> parse $ Unparsed $ substring x 1 (length x - 1)
				_ -> Unparsed x
			) split_strings
parse_brackets _ = error "Run parse_brackets on a parsed token"

parse_characters :: RegularExpression -> [RegularExpression]
parse_characters (Unparsed to_parse) = results
	where
		character_positions = map (\x -> if (x == 'a' || x == 'e') then True else False) to_parse
		change_points = filter (\x -> 
						(character_positions !! x && not (character_positions !! (x - 1))) || 
						(not (character_positions !! x)  && character_positions !! (x - 1))
					) [1 .. (length to_parse - 1)]
		split_strings = split_at to_parse change_points
		results = 
			foldl (\x y -> x ++ y) [] $
			map (\x -> 
				if x !! 0 == 'a' || x !! 0 == 'e' then
					map (\y -> if y == 'a' then Character else NullCharacter) x
				else
					[Unparsed x]
			)
			split_strings

parse_characters _ = error "Run parse_characters on a parsed token"

run_if_unparsed :: (RegularExpression -> [RegularExpression]) -> RegularExpression -> [RegularExpression]
run_if_unparsed parse_step regex@(Unparsed _) = parse_step regex
run_if_unparsed _ regex = [regex]

run_on_unparsed :: (RegularExpression -> [RegularExpression]) -> [RegularExpression] -> [RegularExpression]
run_on_unparsed parse_step regexes = foldl (\x y -> x ++ (run_if_unparsed parse_step y)) [] regexes

unparsed_payload :: RegularExpression -> String
unparsed_payload (Unparsed payload) = payload
unparsed_payload _ = ""

parse_stars :: [RegularExpression] -> [RegularExpression]
parse_stars ((Unparsed "*"):xs) = error "Unparsed star"
parse_stars (first:(Unparsed "*"):xs) = parse_stars $ Star first : xs
parse_stars (x:xs) = x : parse_stars xs
parse_stars [] = []

parse_pluses :: [RegularExpression] -> [RegularExpression]
parse_pluses ((Unparsed "+"):xs) = error "Unparsed plus"
parse_pluses (x:(Unparsed "+"):y:xs) = parse_pluses $ (Plus x y) : xs
parse_pluses (x:xs) = x : parse_pluses xs
parse_pluses [] = []

-- I assume there are only two operators * and + and no characters beside e and a
split_operators :: RegularExpression -> [RegularExpression]
split_operators (Unparsed payload) = map Unparsed $ split_at payload [0 .. length payload]

compose :: [RegularExpression] -> [RegularExpression]
compose ((Composition first):(Composition second):xs) = compose $ Composition (first ++ second) : xs
--compose ((Composition composed):(NullCharacter):xs) = compose $ Composition composed : xs
--compose ((NullCharacter):(Composition composed):xs) = compose $ Composition composed : xs
--compose ((Character):(NullCharacter):xs) =  compose $ Character : xs
--compose ((NullCharacter):(Character):xs) = compose $ Character : xs
compose ((NullCharacter):(NullCharacter):xs) = compose $ NullCharacter : xs
compose (x:unparsed@(Unparsed _):xs) = x : unparsed : compose xs
compose (unprased@(Unparsed _):xs) = unprased : compose xs
compose ((NullCharacter):x:xs) = compose $ x : xs
compose (x:(NullCharacter):xs) = compose $ x : xs
compose ((Composition composed):x:xs) = compose $ (Composition $ composed ++ [x]) : xs
compose (x:y:xs) = compose (Composition [x, y] : xs)
compose (x:[]) = [x]
compose [] = []

parse :: RegularExpression-> RegularExpression
parse (Unparsed string_to_parse) 
	| length result == 1 = head result
	| length result == 0 = error "Empty expression"
	| otherwise = error "Error, parsed to many tokens"
	where
		result =
			parse_pluses
			$ compose
			$ parse_stars 
			$ run_on_unparsed split_operators
			$ run_on_unparsed parse_characters 
			$ parse_brackets (Unparsed string_to_parse) -- debug only
parse _ = error "Run parse on a parsed token"


------------------FSM-converter-------------------

type FiniteStateMachine = [State] 
data State = State {epsilon_transitions :: [Int], alpha_transitions :: [Int]}
	deriving (Show, Eq)

p :: FiniteStateMachine -> IO ()
p x = p_helper x 0

p_helper :: FiniteStateMachine -> Int -> IO ()
p_helper [] _ = return ()
p_helper (x:xs) state_number = 
	putStrLn (show state_number ++ ": " ++ (show $ epsilon_transitions x) ++ " " ++ show (alpha_transitions x)) >>
	(p_helper xs (state_number + 1))

regular_expression_to_finite_state_machine :: RegularExpression -> FiniteStateMachine
regular_expression_to_finite_state_machine regular_expression = α_closure $ ε_closure $ [State [] []] ++ (converter_helper regular_expression 0 1)

count_states :: RegularExpression -> Int
count_states Character = 1
count_states NullCharacter = 1
count_states (Plus first second) = 1 + count_states first + count_states second
count_states (Star argument) = 1 + count_states argument;
count_states (Composition arguments) = foldl (\x y -> x + count_states y) 0 arguments
count_states (Unparsed _) = error "Encountered an unparsed token"

converter_helper :: RegularExpression -> Int -> Int -> FiniteStateMachine
converter_helper Character end_state shift = [State [] [end_state]]
converter_helper NullCharacter end_state shift = [State [end_state] []]
converter_helper (Plus first_argument second_argument) end_state shift = [State [shift + 1, shift + 1 + first_argument_size] []] ++ converter_helper first_argument end_state (shift + 1) ++ converter_helper second_argument end_state (shift + 1 + first_argument_size)
	where
		first_argument_size = count_states first_argument
converter_helper (Star argument) end_state shift = [State [end_state, shift + 1] []] ++ (converter_helper argument shift $ shift + 1)
converter_helper (Composition [x]) end_state shift = error "Composition with 1 arguments"
converter_helper (Composition []) end_state shift = error "Composition with 0 arguments"
converter_helper (Composition arguments) end_state shift = (foldl (++) [] $ map (\x -> converter_helper (arguments !! x) (shift + (collective_size_increment !! (x + 1))) (shift + (collective_size_increment !! x))) [0 .. (length arguments - 2)]) ++ (converter_helper (last arguments) end_state (shift + (collective_size_increment !! (length arguments - 1))))
	where
		argument_machine_sizes = map (\x -> count_states x) arguments
		collective_size_increment = [0] ++ (sub_sums argument_machine_sizes)

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
	| x == y = x : merge xs ys
	| x < y = x : (merge xs (y:ys))
	| otherwise = y : (merge (x:xs) ys)
merge x [] = x
merge [] y = y

-- 03b5
ε_closure :: FiniteStateMachine -> FiniteStateMachine
ε_closure machine 
	| machine == new_machine = new_machine
	| otherwise = ε_closure new_machine
	where
		new_machine = map (\(State inner_epsilon_transitions inner_alpha_transitions) -> (State (merge inner_epsilon_transitions (foldl (merge) [] (map (\x -> epsilon_transitions (machine !! x)) inner_epsilon_transitions))) inner_alpha_transitions)) machine

α_closure :: FiniteStateMachine -> FiniteStateMachine
α_closure machine = map (\(State inner_epsilon_transitions inner_alpha_transitions) -> State inner_epsilon_transitions (merge inner_alpha_transitions (foldl (merge) [] (map (\x -> epsilon_transitions (machine !! x)) inner_alpha_transitions)))) machine

type MetaState = [Int]
get_initial_meta_state :: FiniteStateMachine -> MetaState
get_initial_meta_state machine = filter (\x -> (length $ alpha_transitions (machine !! x)) /= 0 || x == 0) $ merge [1] $ epsilon_transitions $ machine !! 1

execute_on :: FiniteStateMachine -> String -> MetaState
execute_on machine word
	| bad_input = error "bad characters in input"
	| otherwise = foldl (.) id (map (\x -> step machine) [1 .. steps]) (get_initial_meta_state machine)
	where
		stripped_word = filter (\x -> x /= '\n' && x /= ' ') word
		bad_input = 0 /= (length $ filter (\x -> x /= 'a') stripped_word)
		steps = length stripped_word

string_to_machine :: String -> FiniteStateMachine
string_to_machine string = regular_expression_to_finite_state_machine $ parse $ Unparsed string
		
step :: FiniteStateMachine -> MetaState -> MetaState
step machine meta_state = filter (\x -> (length (alpha_transitions (machine !! x))) /= 0 || x == 0) $ foldl merge [] $ map (\x -> merge (epsilon_transitions x) $ alpha_transitions x) $ map (\x -> machine !! x) meta_state

check_generates_full_language :: FiniteStateMachine -> Bool
check_generates_full_language machine = check_generates_full_language_helper machine []

check_generates_full_language_helper :: FiniteStateMachine -> [MetaState] -> Bool
check_generates_full_language_helper machine []
	| accepts_empty = check_generates_full_language_helper machine [initial_state]
	| otherwise = False
	where
		initial_state = get_initial_meta_state machine
		accepts_empty = length initial_state > 0 && initial_state !! 0 == 0
check_generates_full_language_helper machine meta_states 
	| already_visited = True
--	| accepts = trace(show meta_states ++ show "   ----   " ++ show new_state) check_generates_full_language_helper machine (meta_states ++ [new_state])
	| accepts = check_generates_full_language_helper machine (meta_states ++ [new_state])
	| otherwise = False
	where
		new_state = step machine $ last meta_states
		accepts = length new_state /= 0 && (new_state !! 0 == 0)
		already_visited = 0 < (length $ filter (\x -> x == new_state) meta_states)

main :: IO ()
main = do
	arguments <- getArgs
	case arguments of
		[] -> putStrLn "Not enough arguments"
		[x] -> if (check_generates_full_language $ string_to_machine $ filter (\x -> x /= ' ' && x /= '\n') $ x) then putStrLn "Accepts the full language" else putStrLn "Doesn't accept the full language"
		_ -> putStrLn "Too many arguments"
