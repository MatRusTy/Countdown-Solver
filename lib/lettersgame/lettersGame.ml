
let words_arr = Arg.read_arg "lib/lettersgame/word_list.txt"
let all_words = Array.to_list words_arr
module FreqMap = Map.Make(Char)

let update_freq_map (map : int FreqMap.t) (l : char) =
	let increment_opt i_opt =
		if Option.is_some i_opt then
			Some ((Option.get i_opt) + 1)
		else Some (1)
	in
	FreqMap.update l (increment_opt) map

let is_sub_freq f1 f2 = 
	let f2_proper_sub k i =
		let f2_k_opt = FreqMap.find_opt k f2 in
		if Option.is_some f2_k_opt then
			Option.get f2_k_opt < i
		else
			true
	in
	not (FreqMap.exists f2_proper_sub f1)

let resolve (input : char list) : string list = 
	let letters = List.map (Char.lowercase_ascii) input in

	let letters_freq = List.fold_left update_freq_map FreqMap.empty letters in
	let string_freq str = String.fold_left update_freq_map FreqMap.empty str in

	let candidates2 = List.filter
		(fun s -> is_sub_freq (string_freq s) letters_freq)
		all_words
	in

	let sortfunc s1 s2 = String.length s2 - String.length s1 in
	List.fast_sort sortfunc candidates2 
