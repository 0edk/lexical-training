(require hyrule :readers [%])
(import hyrule [inc])
(import itertools random re requests)

; https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
(setv LETTERS (list "eariotnslcudpmhgbfywkvxzjq"))
(setv CONSONANTS (list "qjzxvkwfbghmpdclsntr"))
(setv VOWELS (list "yuoiae"))
(setv HEADERS {})
(setv RARE (re.compile "([a-z]+)<[Bb][Rr]>"))
(setv req-count 0)

(defn weightify [l]
    (list (itertools.chain #* (map
        #%(itertools.repeat (get %1 1) (inc (get %1 0)))
        (enumerate l)))))

(defn ias-url [s]
    (setv proto "https://")
    (setv domain "new.wordsmith.org")
    (setv path "/anagram/anagram.cgi")
    (setv params (+
        f"?anagram={s}&language=english&t=10&d=1&include="
        f"&exclude=&n=&m=&a=n&l=n&q=n&k=0&source=adv"))
    (+ proto domain path params))

(defn get-anagrams [s]
    (global req-count)
    (setv resp (requests.get (ias-url s) :headers HEADERS))
    (+= req-count 1)
    (if (in "No anagrams found." resp.text)
        []
        (let [#(_, words) (.split resp.text "Displaying all:")]
            (re.findall RARE words))))

(defn pick-letters [n]
    (setv consonant-prop 0 vowel-prop 0 word "")
    (for [i (range n)]
        (if (> (random.random) (/ (inc consonant-prop) (inc vowel-prop) 3))
            (setv consonant-prop (inc consonant-prop)
                word (+ word (random.choice (weightify CONSONANTS))))
            (setv vowel-prop (inc vowel-prop)
                word (+ word (random.choice (weightify VOWELS))))))
    word)

(defn gen-puzzle [base-len added-len]
    (if (= added-len 1)
        (while True
            (setv fulls [])
            (while (not (setx fulls (get-anagrams (setx letters
                (pick-letters (+ base-len added-len)))))))
            (for [i (range (len letters))]
                (setv bases (get-anagrams (setx addend
                    (+ (cut letters i) (cut letters (inc i) None)))))
                (setv addend (get letters i))
                (when bases (break)))
            (if bases
                (do (setv bword (random.choice bases)) (break))
                (print f"{fulls} is bad for contraction")))
        (while True
            (setv bases [])
            (while (not (setx bases (get-anagrams (setx addend
                (pick-letters base-len))))))
            (setv bword (random.choice bases))
            (for [t (range (max 12 (* base-len added-len)))]
                (setv fulls (get-anagrams (+ bword (setx addend
                    (pick-letters added-len)))))
                (when fulls (break)))
            (if fulls (break) (print f"{bword} is bad for extension"))))
    #(bword addend fulls))

(defn play-puzzle [base ext fulls]
    (setv letters (list ext))
    (random.shuffle letters)
    (print :end "" :flush True
        f"Anagram {base} + {(.join " " letters)} ({(len fulls)} solutions): ")
    (setv player-guess (.split (.lower (input))))
    (if (and player-guess (in (get player-guess 0) fulls))
        (do
            (if (= (len fulls) 1)
                (print f"Correct, uniquely")
                (do (fulls.remove (get player-guess 0))
                    (print f"Correct, also: {(.join ", " fulls)}")))
            True)
        (do (print f"Wrong, solutions were: {(.join ", " fulls)}") False)))

(defn show-puzzle [base ext fulls]
    (setv letters (list ext))
    (random.shuffle letters)
    (print f"{base} + {(.join " " letters)} ({(len fulls)})"))

(for [i (range 1 4)]
    (for [j (range 3 7)]
        (for [k (range 4)]
            (setv #(b e f) (gen-puzzle j i))
            (setv req-count 0)
            (play-puzzle b e f))))
            ;(show-puzzle b e f))))
