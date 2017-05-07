--import System.IO

-------------------------------------
-------------------------------------
-- Data
-------------------------------------
-------------------------------------

-------------------------------------
-- P2
-------------------------------------

--identificadors
type Ident = String

--expressions numeriques
data NExpr a =
    Var Ident
    | Const a
    | Plus (NExpr a) (NExpr a)
    | Minus (NExpr a) (NExpr a)
    | Times (NExpr a) (NExpr a)
    | Length Ident
    | Diameter Ident
    deriving (Read)

--expressions booleanes
data BExpr a =
    And (BExpr a) (BExpr a)
    | Or (BExpr a) (BExpr a)
    | Not (BExpr a)
    | Gt (NExpr a) (NExpr a)
    | Lt (NExpr a) (NExpr a)
    | Eq (NExpr a) (NExpr a)
    | Empty Ident
    | Full Ident
    deriving (Read)

--expressions de conector
data CExpr a =
    CVar Ident
    | Connector (NExpr a)
    deriving (Read)

--expressions de tub
data TExpr a =
    TVar Ident
    | Merge (TExpr a) (CExpr a) (TExpr a)
    | Tube (NExpr a) (NExpr a)
    deriving (Read)

--comandes/instruccions
data Command a =
    Copy Ident Ident
    | TAssign Ident (TExpr a)
    | CAssign Ident (CExpr a)
    | Input Ident
    | Print (NExpr a)
    | Draw (TExpr a)
    | Seq [Command a]
    | Cond (BExpr a) (Command a) (Command a)
    | Loop (BExpr a) (Command a)
    | DeclareVector Ident (NExpr a)
    | Push Ident Ident
    | Pop Ident Ident
    | Split  Ident Ident Ident
    deriving (Read)

-------------------------------------
-------------------------------------   
--Show Data
-------------------------------------
-------------------------------------

-------------------------------------
-- P2
-------------------------------------
instance Show a => Show (BExpr a) where
    --casos amb parentitzacio
    show (And (Or a b) (Or c d))  = "(" ++ show (Or a b) ++ ")" ++
                                    " AND " ++ "(" ++ show (Or c d) ++ ")"
    show (And (And a b) (Or c d)) = "(" ++ show (And a b) ++ ")" ++
                                    " AND " ++ "(" ++ show (Or c d) ++ ")"
    show (And (And a b) bExp2)    = "(" ++ show (And a b) ++ ")" ++
                                    " AND " ++ show bExp2
    show (And (Or a b) bExp2)     = "(" ++ show (Or a b) ++ ")" ++
                                    " AND " ++ show bExp2
    show (And bExp1 (Or c d))     = show bExp1 ++
                                    " AND " ++ "(" ++ show (Or c d) ++ ")"  
    show (Or (Or a b) bExp2)      = "(" ++ show (Or a b) ++ ")" ++
                                    " OR " ++ show bExp2
    show (Not (And a b))          = "NOT " ++ "(" ++ show (And a b) ++ ")"
    show (Not (Or a b))           = "NOT "++ "(" ++ show (Or a b) ++ ")"
    --casos sense parentitzacio
    show (And bExp1 bExp2)        = show bExp1 ++ " AND " ++ show bExp2
    show (Or bExp1 bExp2)         = show bExp1 ++ " OR " ++ show bExp2
    show (Not bExp1)              = "NOT " ++ show bExp1
    show (Gt nExp1 nExp2)         = show nExp1 ++ " > " ++ show nExp2
    show (Lt nExp1 nExp2)         = show nExp1 ++ " < " ++ show nExp2
    show (Eq nExp1 nExp2)         = show nExp1 ++ " = " ++ show nExp2
    show (Empty id)               = "EMPTY(" ++ id ++ ")"
    show (Full id)                = "FULL(" ++ id ++ ")"

instance Show a => Show (NExpr a) where
    show (Var id)            = id
    show (Const a)           = show a
    show (Plus nExp1 nExp2)  = show nExp1 ++ " + " ++ show nExp2
    show (Minus nExp1 nExp2) = show nExp1 ++ " - " ++ show nExp2
    show (Times nExp1 nExp2) = show nExp1 ++ " * " ++ show nExp2
    show (Length id)         = "LENGTH(" ++ id ++ ")"
    show (Diameter id)       = "DIAMETER(" ++ id ++ ")"

instance Show a => Show (TExpr a) where
    show (TVar id)                   = id
    show (Merge tExpr1 cExpr tExpr2) = "MERGE " ++ show tExpr1 ++
                                       " " ++ show cExpr ++
                                       " " ++ show tExpr2
    show (Tube nExpr1 nExpr2)        = "TUBE " ++ show nExpr1 ++
                                       " " ++ show nExpr2

instance Show a => Show (CExpr a) where
    show (CVar id)         = id
    show (Connector nExpr) = "CONNECTOR " ++ show nExpr

instance Show a => Show (Command a) where
    show a = showCommand 0 a

indentator :: Int -> String
indentator n = replicate (n*2) ' '

showCommand :: Show a => Int -> (Command a) -> String
showCommand n (Copy id1 id2)           = indentator n ++
                                         id1 ++ " = " ++ id2

showCommand n (TAssign id tExpr)       = indentator n ++
                                         id ++ " = " ++ show tExpr

showCommand n (CAssign id tExpr)       = indentator n ++
                                         id ++ " = " ++ show tExpr

showCommand n (Split id1 id2 id3)      = indentator n ++
                                         "(" ++ id1 ++ "," ++ id2 ++ ")" ++
                                         " = SPLIT " ++ id3

showCommand n (Input id)               = indentator n ++
                                         "INPUT " ++ id

showCommand n (Print nExpr)            = indentator n ++
                                         "PRINT " ++ show nExpr

showCommand n (Draw tExpr)             = indentator n ++
                                         "DRAW " ++ show tExpr

showCommand n (Seq cmds)               = foldr ((++) . (\x -> showCommand n x ++ "\n")) [] cmds

showCommand n (Cond bExpr cmds1 cmds2) = indentator n ++
                                         "IF (" ++ show bExpr ++ ")\n" ++
                                         showCommand (n + 1) cmds1 ++
                                         "ELSE\n" ++
                                         showCommand (n + 1) cmds2 ++
                                         "ENDIF"

showCommand n (Loop bExpr cmd)         = indentator n ++
                                         "WHILE (" ++ show bExpr ++ ")\n" ++
                                         showCommand (n + 1) cmd ++
                                         "ENDWHILE"

showCommand n (DeclareVector id nExpr) = indentator n ++
                                         id ++ 
                                         " = TUBEVECTOR OF " ++ show nExpr

showCommand n (Push id1 id2)           = indentator n ++
                                         "PUSH " ++ id1 ++ " " ++ id2

showCommand n (Pop id1 id2)            = indentator n ++
                                         "POP " ++ id1 ++ " " ++ id2


-------------------------------------
-------------------------------------
-- Memory Data
-------------------------------------
-------------------------------------

-------------------------------------
-- P3
-------------------------------------

-- Value
data Val a =
    NVal a
    | TVal [a] a
    | CVal a
    | VTVal [([a], a)] a a -- size actual, vector, max size
    | EmptyVal
    deriving (Show, Eq, Ord)

-- type check
isNVal :: Val a -> Bool
isNVal (NVal _) = True
isNVal _        = False

isTVal :: Val a -> Bool
isTVal (TVal _ _) = True
isTVal _          = False

isCVal :: Val a -> Bool
isCVal (CVal _) = True
isCVal _        = False

isVTVAL :: Val a -> Bool
isVTVAL (VTVal _ _ _) = True
isVTVAL _           = False

isEmptyVal :: Val a -> Bool
isEmptyVal EmptyVal = True
isEmptyVal _        = False

-- get value
fromNVal :: Val a -> a
fromNVal (NVal x) = x
fromNVal _        = undefined

fromTVal :: Val a -> ([a], a)
fromTVal (TVal l d) = (l, d)
fromTVal _          = undefined

fromCVal :: Val a -> a
fromCVal (CVal d) = d
fromCVal _        = undefined

fromVTVal :: Val a -> ([([a], a)], a, a)
fromVTVal (VTVal list size maxSize) = (list, size, maxSize)
fromVTVal _            = undefined

-- TVal functions
lengthTVal :: (Num a) => Val a -> a
lengthTVal (TVal l _) = sum l
lengthTVal _          = undefined

diameterTVal :: (Num a) => Val a -> a
diameterTVal (TVal _ d) = d
diameterTVal _          = undefined

-- CVal functions
diameterCVal :: (Num a) => Val a -> a
diameterCVal = fromCVal

-- VTVal functions
isEmpty :: (Num a, Ord a) => Val a -> Bool
isEmpty (VTVal [] _ _) = True
isEmpty _              = False

isFull :: (Num a, Ord a) => Val a -> Bool
isFull (VTVal _ size maxSize) = size >= maxSize
isFull _                      = undefined


-- Memory Value
data MemVal a =
    KeyVal Ident (Val a)
    deriving Show


-- SymTable
class SymTable m where
    update :: m a -> String -> Val a -> m a
    value  :: m a -> String -> Maybe (Val a)
    start  :: m a


-- Memory List
data MemList a =
    MemMap [MemVal a]
    deriving Show

concatMem :: MemList a -> MemList a -> MemList a
concatMem (MemMap l1) (MemMap l2) = MemMap $ l1 ++ l2

instance SymTable MemList where
    -- llista ordenada de menor a major
    update (MemMap []) key val = MemMap [KeyVal key val]
    update (MemMap (e:se)) key val
        | eKey == key = MemMap (KeyVal key val : se)
        | eKey > key  = MemMap (KeyVal key val : (e : se))
        | otherwise   = concatMem (MemMap [e]) (update (MemMap se) key val)
        where KeyVal eKey eValue = e
    value (MemMap []) key = Nothing
    value (MemMap (e:se)) key
        | eKey == key = Just eValue
        | eKey > key  = Nothing -- com esta ordenada ja podem dir que no esta la clau.
        | otherwise   = value (MemMap se) key
        where KeyVal eKey eValue = e
    start = MemMap []


-- Memory BST
data MemBST a =
    BSTNode (MemVal a) (MemBST a) (MemBST a)
    | BSTEmpty
    deriving Show

instance SymTable MemBST where
    update BSTEmpty key val =
        BSTNode (KeyVal key val) BSTEmpty BSTEmpty
    update (BSTNode tuple leftChild rightChild) key val
        | key == tKey = BSTNode (KeyVal key val) leftChild rightChild
        | key < tKey  = BSTNode tuple (update leftChild key val) rightChild
        | otherwise   = BSTNode tuple leftChild (update rightChild key val)
        where KeyVal tKey tValue = tuple
    value BSTEmpty key = Nothing
    value (BSTNode tuple leftChild rightChild) key
        | key == tKey = Just tValue
        | key < tKey  = value leftChild key
        | otherwise   = value rightChild key
        where KeyVal tKey tValue = tuple
    start = BSTEmpty


-- Memory
data Mem a =
    MemList a
    | MemBST a

emptyMemVars :: SymTable m => m a -> [Ident] -> m a
emptyMemVars mem ids = foldr (\x y -> update y x EmptyVal) mem ids

-------------------------------------
-------------------------------------
-- Errors
-------------------------------------
-------------------------------------

-------------------------------------
-- P3
-------------------------------------

undefinedVarErr :: String
undefinedVarErr = "undefined variable"

noContentErr :: String
noContentErr = "variable content no longer exists"

unmatchedDiameterErr :: String
unmatchedDiameterErr = "unmatched diameter"

emptyVectorErr :: String
emptyVectorErr = "empty vector"

fullVectorErr :: String
fullVectorErr = "full vector"

typeErr :: String
typeErr = "type error"


-------------------------------------
-------------------------------------
-- Semantics
-------------------------------------
-------------------------------------

-------------------------------------
-- P3
-------------------------------------

--evalNExpr: 
-- Evalua, per una memoria (m a) donada, l'expressio numerica (NExpr).
-- Retorna el resultat d'evaluar l'expressio o un missatge d'error.
evalNExpr :: (Num a, Ord a, SymTable m) => m a -> NExpr a -> Either String a
--Var Ident
evalNExpr mem (Var id)
    | isNothing' val             = Left undefinedVarErr
    | isEmptyVal $ fromJust' val = Left noContentErr
    | isNVal $ fromJust' val     = Right $ fromNVal $ fromJust' val
    | otherwise                  = Left typeErr
    where val = value mem id
--Const a
evalNExpr _ (Const n) = Right n
--Plus (NExpr a) (NExpr a)
evalNExpr mem (Plus nExpr1 nExpr2) = evalArithmNExpr mem (+) nExpr1 nExpr2
--Minus (NExpr a) (NExpr a)
evalNExpr mem (Minus nExpr1 nExpr2) = evalArithmNExpr mem (-) nExpr1 nExpr2
--Times (NExpr a) (NExpr a)
evalNExpr mem (Times nExpr1 nExpr2) = evalArithmNExpr mem (*) nExpr1 nExpr2
--Length Ident
evalNExpr mem (Length id)
    | isNothing' val             = Left undefinedVarErr
    | isEmptyVal $ fromJust' val = Left noContentErr
    | isTVal $ fromJust' val     = Right $ lengthTVal $ fromJust' val
    | otherwise                  = Left typeErr
    where val = value mem id
--Diameter Ident
evalNExpr mem (Diameter id)
    | isNothing' val             = Left undefinedVarErr
    | isEmptyVal $ fromJust' val = Left noContentErr
    | isTVal $ fromJust' val     = Right $ diameterTVal $ fromJust' val
    | isCVal $ fromJust' val     = Right $ fromCVal $ fromJust' val
    | otherwise                  = Left typeErr
    where val = value mem id

--evalArithmNExpr: 
-- Evalua, per una memoria (m a) donada, una funcio aritmetica (a->a->a) sobre
-- dues expressions numeriques (NExpr).
-- Retorna el resultat d'evaluar l'expressio o un missatge d'error.
evalArithmNExpr :: (Num a, Ord a, SymTable m) =>
    m a -> (a -> a -> a) -> NExpr a -> NExpr a -> Either String a
evalArithmNExpr mem f nExpr1 nExpr2
    | isLeft' res1 = res1
    | isLeft' res2 = res2
    | otherwise   = Right $ f (fromRight' res1) (fromRight' res2)
    where res1 = evalNExpr mem nExpr1
          res2 = evalNExpr mem nExpr2


--evalBExpr: 
-- Evalua, per una memoria (m a) donada, l'expressio booleana (BExpr).
-- Retorna el resultat d'evaluar l'expressio o un missatge d'error.
evalBExpr :: (Num a, Ord a, SymTable m) => m a -> BExpr a -> Either String Bool
--And (BExpr a) (BExpr a)
evalBExpr mem (And bExpr1 bExpr2) = evalLogicExpr mem (&&) bExpr1 bExpr2
--Or (BExpr a) (BExpr a)
evalBExpr mem (Or bExpr1 bExpr2) = evalLogicExpr mem (||) bExpr1 bExpr2
--Not (BExpr a)
evalBExpr mem (Not bExpr)
    | isLeft' res = res
    | otherwise   = Right $ not $ fromRight' res
    where res = evalBExpr mem bExpr
--Gt (NExpr a) (NExpr a)
evalBExpr mem (Gt nExpr1 nExpr2) = evalCompExpr mem (>) nExpr1 nExpr2
--Lt (NExpr a) (NExpr a)
evalBExpr mem (Lt nExpr1 nExpr2) = evalCompExpr mem (<) nExpr1 nExpr2
--Eq (NExpr a) (NExpr a)
evalBExpr mem (Eq nExpr1 nExpr2) = evalCompExpr mem (==) nExpr1 nExpr2
--Empty Ident
evalBExpr mem (Empty id)
    | isNothing' val             = Left undefinedVarErr
    | isEmptyVal $ fromJust' val = Left noContentErr
    | isVTVAL $ fromJust' val    = Right $ isEmpty $ fromJust' val
    | otherwise                  = Left typeErr
    where val = value mem id
--Full Ident
evalBExpr mem (Full id)
    | isNothing' val             = Left undefinedVarErr
    | isEmptyVal $ fromJust' val = Left noContentErr
    | isVTVAL $ fromJust' val    = Right $ isFull $ fromJust' val
    | otherwise                  = Left typeErr
    where val = value mem id

--evalLogicExpr: 
-- Evalua, per una memoria (m a) donada, una funcio logica (Bool -> Bool -> Bool) sobre
-- dues expressions booleanes (BExpr).
-- Retorna el resultat d'evaluar l'expressio o un missatge d'error.
evalLogicExpr :: (Num a, Ord a, SymTable m) =>
    m a -> (Bool -> Bool -> Bool) -> BExpr a -> BExpr a -> Either String Bool
evalLogicExpr mem f bExpr1 bExpr2
    | isLeft' res1 = res1
    | isLeft' res2 = res2
    | otherwise    = Right $ f (fromRight' res1) (fromRight' res2)
    where res1 = evalBExpr mem bExpr1
          res2 = evalBExpr mem bExpr2

--evalCompExpr: 
-- Evalua, per una memoria (m a) donada, una funcio de comparacio (a->a->Bool) sobre
-- dues expressions numeriques (NExpr).
-- Retorna el resultat d'evaluar l'expressio o un missatge d'error.
evalCompExpr :: (Num a, Ord a, SymTable m) =>
    m a -> (a -> a -> Bool) -> NExpr a -> NExpr a -> Either String Bool
evalCompExpr mem f nExpr1 nExpr2
    | isLeft' res1 = Left $ fromLeft' res1
    | isLeft' res2 = Left $ fromLeft' res2
    | otherwise   = Right $ f (fromRight' res1) (fromRight' res2)
    where res1 = evalNExpr mem nExpr1
          res2 = evalNExpr mem nExpr2


--evalCExpr:
-- Evalua, per una memoria (m a) donada, l'expressio de connector (CExpr).
-- Retorna:
--   1. El resultat d'evaluar l'expressio o un missatge d'error.
--   2. Un llistat dels Identificadors ussats.
evalCExpr :: (Num a, Ord a, SymTable m) =>
    m a -> CExpr a -> (Either String (Val a), [Ident])
--CVar Ident
evalCExpr mem (CVar id)
    | isNothing' val             = (Left undefinedVarErr, [])
    | isEmptyVal $ fromJust' val = (Left noContentErr, [])
    | isCVal $ fromJust' val     = (Right $ fromJust' val, [id])
    | otherwise                  = (Left typeErr, [])
    where val = value mem id
--Connector (NExpr a)
evalCExpr mem (Connector nExpr)
    | isLeft' res = (Left $ fromLeft' res, [])
    | otherwise   = (Right $ CVal $ fromRight' res, [])
    where res = evalNExpr mem nExpr


--evalTExpr:
-- Evalua, per una memoria (m a) donada, l'expressio de tub (TExpr).
-- Retorna:
--   1. El resultat d'evaluar l'expressio o un missatge d'error.
--   2. Un llistat dels Identificadors ussats. En cas d'error sera buida.
evalTExpr :: (Num a, Ord a, SymTable m) =>
    m a -> TExpr a -> (Either String (Val a), [Ident])
--TVar Ident
evalTExpr mem (TVar id)
    | isNothing' val             = (Left undefinedVarErr, [])
    | isEmptyVal $ fromJust' val = (Left noContentErr, [])
    | isTVal $ fromJust' val     = (Right $ fromJust' val, [id])
    | otherwise                  = (Left typeErr, [])
    where val = value mem id
--Merge (TExpr a) (CExpr a) (TExpr a)
evalTExpr mem (Merge tExpr1 cExpr tExpr2)
    | isLeft' $ fst resT1 = (Left $ fromLeft' $ fst resT1, [])
    | isLeft' $ fst resC  = (Left $ fromLeft' $ fst resC, [])
    | isLeft' $ fst resT2 = (Left $ fromLeft' $ fst resT2, [])
    | otherwise           = tryMerge
                                (fromRight' $ fst resT1)
                                (fromRight' $ fst resC)
                                (fromRight' $ fst resT2)
                                ((snd resT1) ++ (snd resC) ++ (snd resT2))
    where resT1 = evalTExpr mem tExpr1
          resC  = evalCExpr mem cExpr
          resT2 = evalTExpr mem tExpr2
--Tube (NExpr a) (NExpr a)
evalTExpr mem (Tube nExpr1 nExpr2)
    | isLeft' res1 = (Left $ fromLeft' res1, [])
    | isLeft' res2 = (Left $ fromLeft' res2, [])
    | otherwise    = (Right $ TVal [fromRight' res1] (fromRight' res2), [])
    where res1 = evalNExpr mem nExpr1
          res2 = evalNExpr mem nExpr2

--tryMerge:
-- intenta fer el merge de dos tubs i un conector (TVal - CVal - TVal).
-- Retorna:
--  1. El TVal resultant o un missatge d'error.
--  2. La llista d'identificadors dels conectors i tubs ussats en el merge.
--     Si hi ha un error la llista sera buida.
tryMerge :: (Num a, Ord a) => 
    Val a -> Val a -> Val a -> [Ident] -> (Either String (Val a), [Ident])
tryMerge t1 c t2 ids
    | diam1 /= diam2 || diam2 /= diam3         = (Left unmatchedDiameterErr, [])
    | otherwise                                =
        (Right $ TVal ((fst $ fromTVal t1) ++ (fst $ fromTVal t2)) diam2, ids)
    where diam1 = diameterTVal t1
          diam2 = diameterCVal c
          diam3 = diameterTVal t2


--interpretCommand: 
-- Interpreta per una memoria (m a) i entrada ([a]) donades un programa (Command).
-- Retorna una tripleta amb:
--   1. La llista de totes les impresions (print i/o draw) o un missatge d'error.
--   2. La memoria resultant.
--   3. Entrada restant despres d'executar el codi.
interpretCommand :: (Num a, Ord a, SymTable m) =>
    m a -> [a] -> Command a -> ((Either String [a]), m a, [a])
--Copy Ident Ident
interpretCommand mem inputList (Copy id1 id2)
    | isNothing' val2             = (Left undefinedVarErr, mem, inputList)
    | isEmptyVal $ fromJust' val2 = (Left noContentErr, mem, inputList)
    | otherwise                   = let newMem = update mem id1 $ fromJust' val2
                                    in (Right [], newMem, inputList)
    where val2 = value mem id2
--TAssign Ident (TExpr a)
interpretCommand mem inputList (TAssign id tExpr)
    | isLeft' resTExpr = (Left $ fromLeft' resTExpr, mem, inputList)
    | otherwise        = let newMem = emptyMemVars mem idList
                         in (Right [], newMem, inputList)
    where (resTExpr, idList) = evalTExpr mem tExpr
--CAssign Ident (CExpr a)
interpretCommand mem inputList (CAssign id cExpr)
    | isLeft' resCExpr = (Left $ fromLeft' resCExpr, mem, inputList)
    | otherwise        = let newMem = emptyMemVars mem idList
                         in (Right [], newMem, inputList)
    where (resCExpr, idList) = evalCExpr mem cExpr
--Input Ident
interpretCommand mem inputList (Input id)
    | null inputList = (Left "No input.", mem, inputList)
    | otherwise      = let newMem = update mem id (NVal inp)
                       in (Right [], newMem, sInp)
    where (inp:sInp) = inputList
--Print (NExpr a)
interpretCommand mem inputList (Print nExpr)
    | isLeft' res = (Left $ fromLeft' res, mem, inputList)
    | otherwise   = (Right $ [fromRight' res], mem, inputList)
    where res = evalNExpr mem nExpr
--Draw (TExpr a)
interpretCommand mem inputList (Draw tExpr)
    | isLeft' res = (Left $ fromLeft' res, mem, inputList)
    | otherwise   = let TVal len diam = fromRight' res
                    in (Right $ [diam] ++ len, mem, inputList)
    where (res, ids) = evalTExpr mem tExpr
--Seq [Command a] -TODO!!!
interpretCommand mem inputList (Seq []) = (Right [], mem, inputList)
interpretCommand mem inputList (Seq (cmd:sCmd))
    | isLeft' cmdEither = (cmdEither, newMem, newInputList)
    | otherwise         = let resSeqTail = interpretCommand newMem newInputList (Seq sCmd)
                              resCmd = (cmdEither, newMem, newInputList)
                          in concatCommantResults resCmd resSeqTail
    where (cmdEither, newMem, newInputList) = interpretCommand mem inputList cmd
--Cond (BExpr a) (Command a) (Command a)
interpretCommand mem inputList (Cond bExpr cmd1 cmd2)
    | isLeft' res    = (Left $ fromLeft' res, mem, inputList)
    | fromRight' res = interpretCommand mem inputList cmd1
    | otherwise      = interpretCommand mem inputList cmd2
    where res = evalBExpr mem bExpr
--Loop (BExpr a) (Command a)
interpretCommand mem inputList (Loop bExpr cmd)
    | isLeft' bRes              = (Left $ fromLeft' bRes, mem, inputList)
    | not $ fromRight' bRes     = (Right [], mem, inputList)
    | isLeft' eitherCmd         = (eitherCmd, newMem, newInputList)
    | otherwise                 =
          let resCmd = (eitherCmd, newMem, newInputList)
              resNextIt = interpretCommand newMem newInputList (Loop bExpr cmd)
          in  concatCommantResults resCmd resNextIt
    where bRes = evalBExpr mem bExpr
          (eitherCmd, newMem, newInputList) = interpretCommand mem inputList cmd
--DeclareVector Ident (NExpr a)
interpretCommand mem inputList (DeclareVector id nExpr)
    | isLeft' res = (Left $ fromLeft' res, mem, inputList)
    | otherwise   = let newMem = update mem id (VTVal [] 0 (fromRight' res))
                    in (Right [], newMem, inputList)
    where res = evalNExpr mem nExpr
--Push Ident Ident
interpretCommand mem inputList (Push id1 id2)
    | isNothing' v || isNothing' t  = (Left undefinedVarErr, mem, inputList)
    | (isEmptyVal $ fromJust' v) || (isEmptyVal $ fromJust' t)
                                    = (Left noContentErr, mem, inputList)
    | not $ (isVTVAL $ fromJust' v) && (isTVal $ fromJust' t)
                                    = (Left typeErr, mem, inputList)
    | isFull $ fromJust' v          = (Left fullVectorErr, mem, inputList)
    | otherwise                     = let TVal tList tLen = fromJust' t
                                          VTVal tVec s ms = fromJust' v
                                          newVec = VTVal ((tList, tLen):tVec) (s+1) ms
                                          newMem = update mem id1 newVec
                                      in (Right [], newMem, inputList)
    where v = value mem id1
          t = value mem id2
--Pop Ident Ident
--Split  Ident Ident Ident
--TODO tot


--concatCommantResults: 
-- Fa un merge dels resultats d'interpretar dues comandes (Command). La segona supossem
-- que es l'ultima que s'ha executat (i per tant la memoria i input resultants seran els
-- d'aquesta).
-- Retorna una tripleta amb:
--   1. La llista de totes les impresions (print i/o draw) o un missatge d'error.
--   2. La memoria resultant.
--   3. Entrada restant despres d'executar el codi.
concatCommantResults :: (Num a, Ord a, SymTable m) => 
    ((Either String [a]), m a, [a]) -> ((Either String [a]), m a, [a]) ->
    ((Either String [a]), m a, [a])
concatCommantResults (either1, mem, inputList) (either2, newMem, newInputList)
    | isLeft' either1 = (either1, mem, inputList)
    | isLeft' either2 = (either2, newMem, newInputList)
    | otherwise       = (Right $ res1 ++ res2, newMem, newInputList)
    where res1 = fromRight' either1
          res2 = fromRight' either2


-------------------------------------
-------------------------------------
-- Helper functions
-------------------------------------
-------------------------------------

-- Maybe
isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' _       = False

fromJust' :: Maybe a -> a
fromJust' (Just x) = x
fromJust' Nothing  = undefined

-- Either
isLeft' :: Either a b -> Bool
isLeft' (Left l) = True
isLeft' _        = False

isRight' :: Either a b -> Bool
isRight' = not . isLeft'

fromLeft' :: Either a b -> a
fromLeft' (Left l) = l
fromLeft' _        = undefined

fromRight' :: Either a b -> b
fromRight' (Right r) = r
fromRight' _         = undefined