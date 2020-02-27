{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where
import Data.List
import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Cell 
			{ tileType :: Char
			, pos :: Position
			} deriving (Eq, Ord)

instance Show Cell where
    show (Cell tileType pos) = [tileType]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level
			{ mapLevel :: A.Array Position Cell
			, blockPos :: [Position]
			, tilesToActivate :: [(Position,[Position])]
			} deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}
insertAtN n y xs = intercalate [y] . groups n $ xs
  where
    groups n xs = takeWhile (not.null) . unfoldr (Just . splitAt n) $ xs

getStatus :: Level -> [Char]
getStatus (Level lvl b _) = status
	where status
		| length b == 1 && tileType == winningTile = "Congrats! You won!\n" 
		| length b == 1 && tileType == softTile = "Game Over\n"
		| length b == 2 && (t1 == emptySpace || t2 == emptySpace) = "Game Over\n"
		| otherwise = ""
			where 
				(Cell tileType _) = (lvl A.! (head b))
				(Cell t1 _) = (lvl A.! (head b))
				(Cell t2 _) = (lvl A.! (head $ tail b))

instance Show Level where
    show (Level mapLevel blockPos tilesToActivate)= "\n" ++ insertAtN (n + 1) '\n' (filter (/= ']') $ filter (/= '[') $ filter (/= ',') (show $ map snd $ A.assocs (mapLevel A.// [(b, (Cell block b)) | b <- blockPos]))) ++ "\n" ++ (getStatus (Level mapLevel blockPos tilesToActivate) )
    	where n = snd $ snd $ A.bounds mapLevel
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel (x, y) (a, b) = Level (A.array ((0, 0), (x, y)) [((i, j), (Cell emptySpace (i, j))) | i <- [0..x], j <- [0..y]]) [(a, b)] []

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile tile (x, y) (Level lvl b tilesToActivate) = Level (lvl A.// [((x, y), (Cell tileT (x, y)))] ) b tilesToActivate
	where tileT
		| tile == 'H' = hardTile
		| tile == 'S' = softTile
		| tile == 'W' = winningTile  

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch switchPos posList (Level lvl b tilesToActivate) = Level (lvl A.// [(switchPos, (Cell switch switchPos))] ) b ((switchPos, posList) : tilesToActivate )

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

getFirstTile :: Position -> Level -> Char
getFirstTile pos (Level lvl _ tilesToActivate) = tile
	where (Cell tile _) = lvl A.! (head $ snd $ head $ (filter ((pos == ).fst) tilesToActivate))

activate :: Cell -> Level -> Level
activate (Cell tileType p) (Level lvl b tilesToActivate) = level
	where level
		| tileType == switch && getFirstTile p (Level lvl b tilesToActivate)  == emptySpace = Level (lvl A.// [(pos, (Cell hardTile pos)) | pos <- (snd  $head$filter ((p== ).fst) tilesToActivate) ] ) b tilesToActivate
		| tileType == switch && getFirstTile p (Level lvl b tilesToActivate)  == hardTile = Level (lvl A.// [(pos, (Cell emptySpace pos)) | pos <- (snd  $head$filter ((p == ).fst) tilesToActivate) ] ) b tilesToActivate
		| otherwise = (Level lvl b tilesToActivate)
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}



getBlockPos :: Level -> Int
getBlockPos (Level _ b _) = blockPos
	where blockPos 
		| length b == 1 = 1
		| length b == 2 = orizontal
			where orizontal
				| (fst $ head b) == (fst $ head $ tail b) = 2
				| (snd $ head b) == (snd $ head $ tail b) = 3 

move1 :: Directions -> Level -> Level
move1 directie (Level lvl b tilesToActivate) = moveDirection
	where moveDirection
		| North == directie = (Level lvl [((fst $ head b) - 2, (snd $ head b)), ((fst $ head b) - 1, (snd $ head b))] tilesToActivate)
		| South == directie = (Level lvl [((fst $ head b) + 1, (snd $ head b)), ((fst $ head b) + 2, (snd $ head b))] tilesToActivate)
		| West == directie = (Level lvl [((fst $ head b), (snd $ head b) -2), ((fst $ head b), (snd $ head b) -1)] tilesToActivate)
		| East == directie = (Level lvl [((fst $ head b), (snd $ head b) +1), ((fst $ head b), (snd $ head b) +2)] tilesToActivate)					

move2 :: Directions -> Level -> Level
move2 directie (Level lvl b tilesToActivate) = moveDirection
	where moveDirection
		| North == directie = (Level lvl [((fst $ head b) - 1, (snd $ head b)), ((fst $ head $ tail b) - 1, (snd $ head $ tail b))] tilesToActivate)
		| South == directie = (Level lvl [((fst $ head b) + 1, (snd $ head b)), ((fst $ head $ tail b) + 1, (snd $ head $ tail b))] tilesToActivate)
		| West == directie = (Level lvl [((fst $ head b), (max (snd $ head b) (snd $ head $ tail b)) -2)] tilesToActivate)
		| East == directie = (Level lvl [((fst $ head b), (max (snd $ head b) (snd $ head $ tail b)) +1)] tilesToActivate)

move3 :: Directions -> Level -> Level
move3 directie (Level lvl b tilesToActivate) = moveDirection
	where moveDirection
		| North == directie = (Level lvl [(((max (fst $ head b) (fst $ head $ tail b)) - 2), (snd $ head b))] tilesToActivate)
		| South == directie = (Level lvl [(((max (fst $ head b) (fst $ head $ tail b)) + 1), (snd $ head b))] tilesToActivate)
		| West == directie = (Level lvl [((fst $ head b), (snd $ head b) - 1), ((fst $ head $ tail b), (snd $ head $ tail b) -1)] tilesToActivate)
		| East == directie = (Level lvl [((fst $ head b), (snd $ head b) + 1), ((fst $ head $ tail b), (snd $ head $ tail b) + 1)] tilesToActivate)


move :: Directions -> Level -> Level
move directie (Level lvl b tilesToActivate) = activate (lvl A.! (head $ blockPos)) nextMove
	where nextMove@(Level l blockPos tiles) 
		| 1 == getBlockPos (Level lvl b tilesToActivate) = move1 directie (Level lvl b tilesToActivate) 
		| 2 == getBlockPos (Level lvl b tilesToActivate) = move2 directie (Level lvl b tilesToActivate)
		| 3 == getBlockPos (Level lvl b tilesToActivate) = move3 directie (Level lvl b tilesToActivate)
{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame = undefined

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors level = [(x, move x level) | x <- [North, South, West, East], getStatus (move x level) == "" ]
    	-- where l = move x level

    isGoal l
    	| getStatus l == "Congrats! You won!\n" = True
    	| otherwise = False

    -- Doar petru BONUS
    -- heuristic = undefined


