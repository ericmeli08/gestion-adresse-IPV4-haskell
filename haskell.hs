type AdressIp = [Int]
type Octet = [Int]

bin :: Int -> Octet
bin x 
    | x<2 = [x]
    | otherwise = bin (div x 2) ++ [mod x 2] 

verify :: AdressIp -> Bool
verify [] = True
verify (x:xs) = (x>=0 && x<=255) && verify xs

norme :: Octet -> Int -> Octet
norme x 0 = x 
norme x n = 0:(norme x (n-1))

norme8 :: Octet -> Octet
norme8 [] = error "not binary"
norme8 x 
        | length x == 8 = x 
        | length x>8 = error "more than 8"
        | otherwise = norme x (8-length x)

isAdress :: AdressIp -> Bool
isAdress x = (length x == 4) && verify x

classe :: AdressIp -> [Char] 
classe (x:xs)
            | chk && (nob !! 0 == 0) = "A"
            | chk && (nob !! 0 == 1) && (nob !! 1 == 0) = "B"
            | chk && (nob !! 0 == 1) && (nob !! 1 == 1) = "C"
            | otherwise = error " unknow"
            where chk = isAdress (x:xs)
                  nob = (norme8 (bin x)) 

mask :: AdressIp -> AdressIp
mask x 
    | classe x == "A" = [255,0,0,0]
    | classe x == "B" = [255,255,0,0]
    | classe x == "C" = [255,255,255,0]
    | otherwise = error "unknow"