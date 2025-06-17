(require hyrule :readers [%])
(import hyrule [inc])
(import itertools random re requests)

; https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
(setv LETTERS (list "eariotnslcudpmhgbfywkvxzjq"))
(setv CONSONANTS (list "qjzxvkwfbghmpdclsntr"))
(setv VOWELS (list "yuoiae"))
(setv HEADERS { "User-Agent" "contact@dkl9.net" })
(setv RARE (re.compile "([a-z]+)<[Bb][Rr]>"))
(setv req-count 0)

(defn weightify [l]
    (list (itertools.chain #* (map #%(itertools.repeat (get %1 1) (inc (get %1 0))) (enumerate l)))))

(defn ias-url [s]
    (setv proto "https://")
    (setv dn "new.wordsmith.org")
    (setv path "/anagram/anagram.cgi")
    (setv params f"?anagram={s}&language=english&t=10&d=1&include=&exclude=&n=&m=&a=n&l=n&q=n&k=0&source=adv")
    (+ proto dn path params))


(defn get-anagrams [s]
    (global req-count)
    (setv resp (requests.get (ias-url s) :headers HEADERS))
    (+= req-count 1)
    (if (in "No anagrams found." resp.text)
        []
        (let [sr (.split resp.text "Displaying all:")]
            (assert (= (len sr) 2))
            (re.findall RARE (get sr 1)))))

(defn pick-letters [n]
    (setv cp 0 vp 0 rw "")
    (for [i (range n)]
        (if (> (random.random) (/ (inc cp) (inc vp) 3))
            (do (setv cp (inc cp) rw (+ rw (random.choice (weightify CONSONANTS)))))
            (do (setv vp (inc vp) rw (+ rw (random.choice (weightify VOWELS)))))))
    rw)

(defn gen-puzzle [n k]
    (if (= k 1)
        (while True
            (setv ewl [])
            (while (not (setx ewl (get-anagrams (setx sew (pick-letters (+ n k)))))))
            (for [i (range (len sew))]
                (setv bwl (get-anagrams (setx tw (+ (cut sew i) (cut sew (inc i) None)))))
                (setv tw (get sew i))
                (when bwl (break)))
            (if bwl
                (do (setv bword (random.choice bwl)) (break))
                (print f"{ewl} is bad for contraction")))
        (while True
            (setv bwl [])
            (while (not (setx bwl (get-anagrams (setx tw (pick-letters n))))))
            (setv bword (random.choice bwl))
            (for [t (range (max 12 (* n k)))]
                (setv ewl (get-anagrams (+ bword (setx tw (pick-letters k)))))
                (when ewl (break)))
            (if ewl (break) (print f"{bword} is bad for extension"))))
    #(bword tw ewl))

(defn play-puzzle [base ext full]
    (setv lel (list ext))
    (random.shuffle lel)
    (print f"Anagram {base} + {(.join " " lel)} ({(len full)} solutions): ")
    (setv pg (.split (.lower (input))))
    (if (and pg (in (get pg 0) full))
        (do
            (if (= (len full) 1)
                (print f"Correct, uniquely")
                (do (full.remove (get pg 0)) (print f"Correct, also: {(.join ", " full)}")))
            True)
        (do (print f"Wrong, solutions were: {(.join ", " full)}") False)))

(defn show-puzzle [base ext full]
    (setv lel (list ext))
    (random.shuffle lel)
    (print f"{base} + {(.join " " lel)} ({(len full)})"))

(for [i (range 1 4)]
    (for [j (range 3 7)]
        (for [k (range 4)]
            (setv #(b e f) (gen-puzzle j i))
            (setv req-count 0)
            (play-puzzle b e f))))
            ;(show-puzzle b e f))))
