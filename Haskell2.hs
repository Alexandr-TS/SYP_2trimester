import Data.List.Split

data BinTree = 
    None
    | Node (String, BinTree, BinTree)
instance Show BinTree where 
    show None = ""
    show (Node(x, None, None)) = show x
    show (Node(x, None, r)) = show x ++ "(" ++ show r ++ ")"
    show (Node(x, l, None)) = "(" ++ show l ++ ")" ++ show x
    show (Node(x, l, r)) = "(" ++ show l ++ ")" ++ show x ++ "(" ++ show r ++ ")"


buildTree s =
    merge1 (map build2 blocks1) ops where
        blocks1 = splitOneOf "+-" s
        ops = filter (\x -> x /= "") (splitOneOf "0123456789*/" s) 
        popBack lst =
            reverse(tail(reverse(lst)))
        merge1 nums [] = head nums
        merge1 nums ops = Node(last ops, merge1 (popBack nums) (popBack ops), last nums)
        build2 s = 
            merge2 nums ops where
                nums = splitOneOf "*/" s
                ops = filter (\x -> x /= "") (splitOneOf ['0'..'9'] s) 
                merge2 nums [] = Node(head nums, None, None)
                merge2 nums ops = Node(last ops, merge2 (popBack nums) (popBack ops), Node(last nums, None, None))

--buildTree "22*3-23*2-1"

rpn None = []
rpn (Node(x, l, r)) = rpn l ++ rpn r ++ [x]

--rpn (buildTree "22*3-23*2-1")

calcTree (Node("+", l, r)) = calcTree l + calcTree r
calcTree (Node("-", l, r)) = calcTree l - calcTree r
calcTree (Node("*", l, r)) = calcTree l * calcTree r
calcTree (Node("/", l, r)) = calcTree l `div` calcTree r
calcTree (Node(x, l, r)) = (read x :: Integer)

calc pol =
    calc2 [] pol where
        calc2 stc [] = toInt(head stc)
        calc2 stc pol = 
            if (h == "+") then calc2(upd (+) stc) t
            else if (h == "-") then calc2(upd (-) stc) t
            else  if (h == "*") then calc2(upd (*) stc) t
            else if (h == "/") then calc2(upd div stc) t
            else calc2 (stc ++ [h]) t
            where 
                h = head pol
                t = tail pol
                upd ope stc = (take ((length stc) - 2) stc) ++ [toStr(ope (toInt(stc !! ((length stc) - 2))) (toInt(last stc)))]
        toInt s =
            read s :: Integer
        toStr i = 
            show i
-- calc ["2","2","+","3","*","6","5","-","/"] 
-- (2 + 2) * 3 / (6 - 5) = 12
-- calc(rpn(buildTree "22*3-23*2-1"))
