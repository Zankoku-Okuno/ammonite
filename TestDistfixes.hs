import Language.Distfix

add = distfix OpenLeft [(== "+")]
cat = distfix OpenRight [(=="++")]
inc = distfix HalfOpenRight [(=="++")]
lt = distfix OpenNon [(=="<")]
ltlt = distfix OpenNon [(=="<"), (=="<")]
fac = distfix HalfOpenLeft [(=="!")]
ite = distfix HalfOpenRight [(=="if"), (=="then"), (=="else")]
mag = distfix Closed [(=="|"), (=="|")]
tup = distfix Closed [(=="<"), (==">")]
braket = distfix Closed [(=="<"), (=="|"), (==">")]

main = do
    test [add] ["a", "+", "b", "+", "c"]
    test [add] ["+", "c"]
    test [cat] ["a", "++", "b", "++", "c"]
    test [lt] ["a", "<", "b", "c"]
    test [lt] ["a", "<", "b", "<", "c"]
    test [fac] ["a", "!", "!", ".foo"]
    test [ite] ["if", "p1", "then", "c1", "else", "if", "p2", "then", "c2", "else", "alt"]
    test [mag] ["|", "a", "|", "|", "b", "|" ]
    test [lt, ltlt] ["a", "<", "b", "<", "c"]
    test [mag, tup, braket] ["<", "|", "a", "|", "|", "b", ">"]
    test [cat, inc] ["++", "a", "++", "c"]

test :: [Distfix String] -> [String] -> IO ()
test dfs text = print $ findDistfixes dfs text