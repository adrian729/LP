import System.IO 
import System.Random


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
    deriving Read

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
    deriving Read

--expressions de conector
data CExpr a =
    CVar Ident
    | Connector (NExpr a)
    deriving Read

--expressions de tub
data TExpr a =
    TVar Ident
    | Merge (TExpr a) (CExpr a) (TExpr a)
    | Tube (NExpr a) (NExpr a)
    deriving Read

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
    deriving Read

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
    show (And bExp1@(Or a b) bExp2@(Or c d))  = "(" ++ show bExp1 ++ ")" ++
                                                " AND " ++ "(" ++ show bExp2 ++ ")"
    show (And bExp1@(And a b) bExp2@(Or c d)) = "(" ++ show bExp1 ++ ")" ++
                                                " AND " ++ "(" ++ show bExp2 ++ ")"
    show (And bExp1@(And a b) bExp2)          = "(" ++ show bExp1 ++ ")" ++
                                                " AND " ++ show bExp2
    show (And bExp1@(Or a b) bExp2)           = "(" ++ show bExp1 ++ ")" ++
                                                " AND " ++ show bExp2
    show (And bExp1 bExp2@(Or c d))           = show bExp1 ++
                                                " AND " ++ "(" ++ show bExp2 ++ ")"  
    show (Or bExp1@(Or a b) bExp2)            = "(" ++ show bExp1 ++ ")" ++
                                                " OR " ++ show bExp2
    show (Not bExp@(And a b))                 = "NOT " ++ "(" ++ show bExp ++ ")"
    show (Not bExp@(Or a b))                  = "NOT "++ "(" ++ show bExp ++ ")"
    --casos sense parentitzacio
    show (And bExp1 bExp2)                    = show bExp1 ++ " AND " ++ show bExp2
    show (Or bExp1 bExp2)                     = show bExp1 ++ " OR " ++ show bExp2
    show (Not bExp)                           = "NOT " ++ show bExp
    show (Gt nExp1 nExp2)                     = show nExp1 ++ " > " ++ show nExp2
    show (Lt nExp1 nExp2)                     = show nExp1 ++ " < " ++ show nExp2
    show (Eq nExp1 nExp2)                     = show nExp1 ++ " == " ++ show nExp2
    show (Empty id)                           = "EMPTY(" ++ id ++ ")"
    show (Full id)                            = "FULL(" ++ id ++ ")"



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
                                         showCommand (succ n) cmds1 ++
                                         "ELSE\n" ++
                                         showCommand (succ n) cmds2 ++
                                         "ENDIF"
showCommand n (Loop bExpr cmd)         = indentator n ++
                                         "WHILE (" ++ show bExpr ++ ")\n" ++
                                         showCommand (succ n) cmd ++
                                         "ENDWHILE"
showCommand n (DeclareVector id nExpr) = indentator n ++
                                         id ++ " = TUBEVECTOR OF " ++ show nExpr
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
    | TVal [a] a -- size sub-tubs compossen tub, diametre
    | CVal a -- diametre
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
isVTVAL _             = False

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


-- SymTable'
class SymTable' m where
    sortedListMem :: m a -> [(String, Val a)] -- donada una estructura amb forma de diccionari 
                                              -- de (Val a), retorna una llista ordenada amb 
                                              -- tuples clau - contingut. Ordenacio no decreixent
                                              -- per clau.

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

-- Llista ja ordenada
instance SymTable' MemList where
    sortedListMem (MemMap []) = []
    sortedListMem (MemMap (e:se)) = (eKey, eValue):(sortedListMem $ MemMap se)
        where KeyVal eKey eValue = e


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

instance SymTable' MemBST where
    sortedListMem treeBST = inOrder treeBST -- BST tornara ordenat creixentment al fer inordre

inOrder :: MemBST a -> [(String, Val a)]
inOrder BSTEmpty = []
inOrder (BSTNode tuple l r) = inOrder l ++ [(tKey, tValue)] ++ inOrder r
    where KeyVal tKey tValue = tuple


-- Memory
data Mem a =
    MemList a
    | MemBST a


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

wrongSizeErr :: String
wrongSizeErr = "size should be more than zero"


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
-- Evalua, per una memoria (m a) donada, una funcio aritmetica (a -> a -> a) sobre
-- dues expressions numeriques (NExpr).
-- Retorna el resultat d'evaluar l'expressio o un missatge d'error.
evalArithmNExpr :: (Num a, Ord a, SymTable m) =>
    m a -> (a -> a -> a) -> NExpr a -> NExpr a -> Either String a
evalArithmNExpr mem f nExpr1 nExpr2
    | isLeft' res1 = res1
    | isLeft' res2 = res2
    | otherwise    = Right $ f (fromRight' res1) (fromRight' res2)
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
    | otherwise    = Right $ f (fromRight' res1) (fromRight' res2)
    where res1 = evalNExpr mem nExpr1
          res2 = evalNExpr mem nExpr2


--evalCExpr:
-- Evalua, per una memoria (m a) donada, l'expressio de connector (CExpr).
-- Retorna:
--   1. El resultat d'evaluar l'expressio o un missatge d'error.
--   2. La memoria despres d'ussar l'expressio.
evalCExpr :: (Num a, Ord a, SymTable m) =>
    m a -> CExpr a -> (Either String (Val a), m a)
--CVar Ident
evalCExpr mem (CVar id)
    | isNothing' val             = (Left undefinedVarErr, mem)
    | isEmptyVal $ fromJust' val = (Left noContentErr, mem)
    | isCVal $ fromJust' val     = let newMem = update mem id EmptyVal
                                   in (Right $ fromJust' val, newMem)
    | otherwise                  = (Left typeErr, mem)
    where val = value mem id
--Connector (NExpr a)
evalCExpr mem (Connector nExpr)
    | isLeft' res = (Left $ fromLeft' res, mem)
    | otherwise   = (Right $ CVal $ fromRight' res, mem)
    where res = evalNExpr mem nExpr


--evalTExpr:
-- Evalua, per una memoria (m a) donada, l'expressio de tub (TExpr).
-- Retorna:
--   1. El resultat d'evaluar l'expressio o un missatge d'error.
--   2. La memoria despres d'ussar l'expressio.
evalTExpr :: (Num a, Ord a, SymTable m) =>
    m a -> TExpr a -> (Either String (Val a), m a)
--TVar Ident
evalTExpr mem (TVar id)
    | isNothing' val             = (Left undefinedVarErr, mem)
    | isEmptyVal $ fromJust' val = (Left noContentErr, mem)
    | isTVal $ fromJust' val     = let newMem = update mem id EmptyVal
                                   in (Right $ fromJust' val, newMem)
    | otherwise                  = (Left typeErr, mem)
    where val = value mem id
--Merge (TExpr a) (CExpr a) (TExpr a)
evalTExpr mem (Merge tExpr1 cExpr tExpr2)
    | isLeft' $ eitherT1 = (Left $ fromLeft' $ eitherT1, mem)
    | isLeft' $ eitherC  = (Left $ fromLeft' $ eitherC, mem)
    | isLeft' $ eitherT2 = (Left $ fromLeft' $ eitherT2, mem)
    | otherwise          = tryMerge
                               (fromRight' $ eitherT1)
                               (fromRight' $ eitherC)
                               (fromRight' $ eitherT2)
                               newMem
    where (eitherT1, memT1)  = evalTExpr mem tExpr1
          (eitherC, memC)    = evalCExpr memT1 cExpr
          (eitherT2, newMem) = evalTExpr memC tExpr2
--Tube (NExpr a) (NExpr a)
evalTExpr mem (Tube nExpr1 nExpr2)
    | isLeft' res1 = (Left $ fromLeft' res1, mem)
    | isLeft' res2 = (Left $ fromLeft' res2, mem)
    | fromRight' res1 <= 0 || fromRight' res2 <= 0
                   = (Left wrongSizeErr, mem)
    | otherwise    = (Right $ TVal [fromRight' res1] (fromRight' res2), mem)
    where res1 = evalNExpr mem nExpr1
          res2 = evalNExpr mem nExpr2

--tryMerge:
-- intenta fer el merge de dos tubs i un conector (TVal - CVal - TVal).
-- Retorna:
--  1. El TVal resultant o un missatge d'error.
--  2. La llista d'identificadors dels conectors i tubs ussats en el merge.
--     Si hi ha un error la llista sera buida.
tryMerge :: (Num a, Ord a) => 
    Val a -> Val a -> Val a -> m a -> (Either String (Val a), m a)
tryMerge t1 c t2 mem
    | diam1 /= diam2 || diam2 /= diam3         = (Left unmatchedDiameterErr, mem)
    | otherwise                                =
        (Right $ TVal ((fst $ fromTVal t1) ++ (fst $ fromTVal t2)) diam2, mem)
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
    | otherwise        = let newMem = update memTExpr id $ fromRight' resTExpr
                         in (Right [], newMem, inputList)
    where (resTExpr, memTExpr) = evalTExpr mem tExpr
--CAssign Ident (CExpr a)
interpretCommand mem inputList (CAssign id cExpr)
    | isLeft' resCExpr = (Left $ fromLeft' resCExpr, mem, inputList)
    | otherwise        = let newMem = update memCExpr id $ fromRight' resCExpr
                         in (Right [], newMem, inputList)
    where (resCExpr, memCExpr) = evalCExpr mem cExpr
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
    where (res, _) = evalTExpr mem tExpr
--Seq [Command a] --TODO: Repassar be que funcioni amb diferents exemples.
interpretCommand mem inputList (Seq []) = (Right [], mem, inputList)
interpretCommand mem inputList (Seq (cmd:sCmd))
    | isLeft' cmdEither = (cmdEither, mem, inputList)
    | otherwise         = let resSeqTail = interpretCommand newMem newInputList (Seq sCmd)
                              resCmd = (cmdEither, newMem, newInputList)
                          in concatCommantResults resCmd resSeqTail
    where (cmdEither, newMem, newInputList) = interpretCommand mem inputList cmd
--Cond (BExpr a) (Command a) (Command a) --TODO: Repassar be que funcioni amb diferents exemples.
interpretCommand mem inputList (Cond bExpr cmd1 cmd2)
    | isLeft' res    = (Left $ fromLeft' res, mem, inputList)
    | fromRight' res = interpretCommand mem inputList cmd1
    | otherwise      = interpretCommand mem inputList cmd2
    where res = evalBExpr mem bExpr
--Loop (BExpr a) (Command a) --TODO: Repassar be que funcioni amb diferents exemples.
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
--DeclareVector Ident (NExpr a) --TODO: Repassar be que funcioni amb diferents exemples.
interpretCommand mem inputList (DeclareVector id nExpr)
    | isLeft' res        = (Left $ fromLeft' res, mem, inputList)
    | fromRight' res < 1 = (Left $ wrongSizeErr, mem, inputList)
    | otherwise          = let newMem = update mem id (VTVal [] 0 (fromRight' res))
                           in (Right [], newMem, inputList)
    where res = evalNExpr mem nExpr
--Push Ident Ident --TODO: Repassar be que funcioni amb diferents exemples.
interpretCommand mem inputList (Push idv idt)
    | isNothing' v || isNothing' t  = (Left undefinedVarErr, mem, inputList)
    | (isEmptyVal $ fromJust' v) || (isEmptyVal $ fromJust' t)
                                    = (Left noContentErr, mem, inputList)
    | not $ (isVTVAL $ fromJust' v) && (isTVal $ fromJust' t)
                                    = (Left typeErr, mem, inputList)
    | isFull $ fromJust' v          = (Left fullVectorErr, mem, inputList)
    | otherwise                     = let TVal tList tDiam = fromJust' t
                                          VTVal tVec s ms = fromJust' v
                                          newVec = VTVal ((tList, tDiam):tVec) (s+1) ms
                                          newMem = update mem idv newVec
                                      in (Right [], newMem, inputList)
    where v = value mem idv
          t = value mem idt
--Pop Ident Ident --TODO: Repassar be que funcioni amb diferents exemples.
interpretCommand mem inputList (Pop idv idt)
    | isNothing' v                = (Left undefinedVarErr, mem, inputList)
    | isEmptyVal $ fromJust' v    = (Left noContentErr, mem, inputList)
    | not $ isVTVAL $ fromJust' v = (Left typeErr, mem, inputList)
    | isEmpty $ fromJust' v       = (Left emptyVectorErr, mem, inputList)
    | otherwise                   = let VTVal ((tList, tDiam):tVec) s ms = fromJust' v
                                        newVec = VTVal tVec (s-1) ms
                                        newTube = TVal tList tDiam
                                        newMemVec = update mem idv newVec
                                        newMem = update mem idt newTube
                                      in (Right [], newMem, inputList)
    where v = value mem idv
--Split  Ident Ident Ident --TODO: Repassar be que funcioni amb diferents exemples.
interpretCommand mem inputList (Split idlt idrt idt)
    | isNothing' t                    = (Left undefinedVarErr, mem, inputList)
    | isEmptyVal $ fromJust' t        = (Left noContentErr, mem, inputList)
    | not $ isTVal $ fromJust' t      = (Left typeErr, mem, inputList)
    | (lengthTVal $ fromJust' t) <= 1 = (Left wrongSizeErr, mem, inputList)
    | otherwise                       = let tlen = lengthTVal $ fromJust' t
                                            leftLen = tlen - (part2 tlen)
                                            (tl, tr) = splitTube (fromJust' t) leftLen
                                            eraseTMem = update mem idt EmptyVal
                                            tlMem = update eraseTMem idlt tl
                                            newMem = update tlMem idrt tr
                                        in (Right [], newMem, inputList)                    
    where t = value mem idt

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
concatCommantResults p1@(either1, mem, inputList) p2@(either2, newMem, newInputList)
    | isLeft' either1 = p1
    | isLeft' either2 = p2
    | otherwise       = (Right $ res1 ++ res2, newMem, newInputList)
    where res1 = fromRight' either1
          res2 = fromRight' either2

--part2:
-- Donat un nombre x.
-- Retorna el nombre sense decimals més gran pel qual el seu doble es menor o igual que x.
part2 :: (Num a, Ord a) => a -> a -- TODO: Implementacio MOLT ineficient, buscar una millor
part2 x = part2' x 0

--part2':
-- Donat un nombre x i un nombre m.
-- Retorna m si el doble d'ell mes 1 es major que x, si no fa una crida recursiva a ella
-- mateixa amb m + 1.
part2' :: (Num a, Ord a) => a -> a -> a -- TODO: Implementacio MOLT ineficient, buscar una millor
part2' x m
    | m' * 2 > x = m
    | otherwise  = part2' x m'
    where m' = m + 1

--splitTube:
-- Donades una variable de tub i un nombre l.
-- Parteix el tub en dos tubs, el primer amb longitud l i l'altre amb la resta.
-- Els retorna en una tupla.
splitTube :: (Num a, Ord a) => Val a -> a -> (Val a, Val a)
splitTube p1@(TVal [] diam) _ = (p1, p1)
splitTube p1@(TVal (t:st) diam) leftLen
    | leftLen <= 0    = (TVal [] diam, p1)
    | t == leftLen    = (TVal [leftLen] diam, TVal st diam)
    | t > leftLen     = let tr = t - leftLen
                        in (TVal [leftLen] diam, TVal (tr:st) diam)
    | otherwise       = let leftLen' = leftLen - t
                            (TVal ll dl, t2) = splitTube (TVal st diam) leftLen'
                        in (TVal (t:ll) diam, t2)


interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram inputList cmd = res
    where (res, _, _) = interpretCommand (start :: MemBST a) inputList cmd
-- Definit aixi, s'ha de canviar manualment si es vol ussar un BST o una llista
-- o comentar/eliminar una de les instancies de SymTable (i no caldra ja dir quin
-- estem ussant).


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


-------------------------------------
-------------------------------------
-- Files & IO
-------------------------------------
-------------------------------------

-------------------------------------
-- P4
-------------------------------------

programhs1 :: String
programhs1 = "programhs1.txt"

programhs2 :: String
programhs2 = "programhs2.txt"

readProgram :: FilePath -> IO String
readProgram file = do ha <- openFile file ReadMode
                      program <- hGetLine ha
                      hClose ha
                      return program

uploadProgram :: Read a => FilePath -> IO (Command a)
uploadProgram fileName = do strPrg <- readProgram fileName
                            let program = (read strPrg :: Read a => Command a)
                            return program


-------------------------------------
-------------------------------------
-- Program
-------------------------------------
-------------------------------------

-------------------------------------
-- P4
-------------------------------------
countDiff :: Eq a => [(Either String [a], Either String [a])] -> Integer
countDiff [] = 0
countDiff ((res1, res2):sres)
    | res1 == res2 = countDiff sres
    | otherwise    = countDiff sres + 1

sortedListMemDiff :: Show a =>
    [(String, Val a)] -> [(String, Val a)] -> [(String, String, String)]
sortedListMemDiff [] [] = []
sortedListMemDiff ((k,v):sm) [] = [(k, show v, "Nothing")] ++ sortedListMemDiff sm []
sortedListMemDiff [] ((k,v):sm) = [(k, "Nothing", show v)] ++ sortedListMemDiff [] sm
sortedListMemDiff p1@((k1,v1):sm1) p2@((k2,v2):sm2)
    | k1 < k2   = [(k1, show v1, "Nothing")] ++ sortedListMemDiff sm1 p2
    | k1 > k2   = [(k2, "Nothing", show v2)] ++ sortedListMemDiff p1 sm2
    | otherwise = [(k1, show v1, show v2)] ++ sortedListMemDiff sm1 sm2

showDiffMem :: (Show a, Num a, Ord a, Show (m a), SymTable m, SymTable' m) =>
    m a -> m a -> IO ()
showDiffMem mem1 mem2 = do
    let slmem1 = sortedListMem mem1
    let slmem2 = sortedListMem mem2
    putStrLn $ show $ sortedListMemDiff slmem1 slmem2

showProgramOutput :: Show a => Either String [a] -> IO ()
showProgramOutput either
    | isLeft' either = do putStrLn $ fromLeft' either
    | otherwise = do putStrLn $ show $ fromRight' either

execProgram :: (Num a, Ord a, SymTable m) =>
    m a -> [a] -> Command a -> ((Either String [a]), m a)
execProgram mem inputList prog = (res, resMem)
    where (res, resMem, _) = interpretCommand mem inputList prog

generateExecution :: (Random a, Show a, Num a, Ord a, Show (m a), SymTable m, SymTable' m) =>
    [a] -> Command a -> Command a -> m a -> IO (Either String [a], Either String [a])
generateExecution inputList prog1 prog2 mem = do
    g <- newStdGen
    let randomList = (randomRs (1, 10) g :: (Num a, Random a) => [a])
    let execInputList = inputList ++ randomList
    let (res1, resMem1) = execProgram mem execInputList prog1
    let (res2, resMem2) = execProgram mem execInputList prog2
    putStrLn "- Resultats del primer programa: "
    showProgramOutput res1
    putStrLn "- Resultats del segon programa: "
    showProgramOutput res2
    putStrLn "- Diferencies entre les memories dels dos programes."
    putStrLn "  (en cas de no existir la variable, Nothing)"
    showDiffMem resMem1 resMem2
    return (res1, res2)

execAutoKTimes :: (Random a, Show a, Num a, Ord a, Show (m a), SymTable m, SymTable' m) =>
    Integer -> Integer -> Command a -> Command a ->
    m a -> IO [(Either String [a], Either String [a])]
execAutoKTimes _ 0 _ _ _ = do return []
execAutoKTimes n k prog1 prog2 mem = do
    putStrLn $ "Executant test " ++ (show n)
    (res1, res2) <- generateExecution [] prog1 prog2 mem
    putStrLn $ replicate 24 '-' ++ "\n"
    nextItRes <- execAutoKTimes (n + 1) (k - 1) prog1 prog2 mem
    return $ (res1, res2):nextItRes

execAuto :: (Random a, Read a, Show a, Num a, Ord a, Show (m a), SymTable m, SymTable' m) =>
    Command a -> Command a -> m a -> IO () -- TODO
execAuto prog1 prog2 mem = do
    putStrLn $ "- Tria quans testos diferents vols passar"
    k <- (readLn :: IO Integer)
    results <- execAutoKTimes 1 k prog1 prog2 mem
    putStrLn "- Execucions que han donat resultats diferents"
    putStrLn "  entre el primer i segon programa"
    putStrLn $ show $ countDiff results

execManual :: (Random a, Read a, Show a, Num a, Ord a, Show (m a), SymTable m, SymTable' m) =>
    String -> Command a -> Command a -> m a -> IO ()
execManual numType prog1 prog2 mem = do
    putStrLn $ "- Introdueix una llista (" ++ numType ++ ")"
    putStrLn "  exemple: [2,3,5]"
    inputList <- (readLn :: Read a => IO [a])
    generateExecution inputList prog1 prog2 mem
    return ()

chooseTest :: (Random a, Read a, Show a, Num a, Ord a, Show (m a), SymTable m, SymTable' m) =>
    String -> Command a -> Command a -> m a -> IO ()
chooseTest numType prog1 prog2 mem = do
    putStrLn "- Tria el tipus de test (0 o 1)"
    putStrLn "  0 - Test comparatiu manual"
    putStrLn "  1 - Test comparatiu automatic"
    n <- readLn
    if n == 0
    then do putStrLn "Has triat test comparatiu manual (0)"
            execManual numType prog1 prog2 mem
    else if n == 1
    then do putStrLn "Has triat test comparatiu automatic (1)"
            execAuto prog1 prog2 mem
    else do putStrLn "Has d'introduir 0 o 1"
            chooseTest numType prog1 prog2 mem

chooseMem :: (Random a, Read a, Show a, Num a, Ord a) =>
    String -> Command a -> Command a -> IO ()
chooseMem numType prog1 prog2 = do
    putStrLn "- Tria el tipus de memoria (0 o 1)"
    putStrLn "  0 - llista"
    putStrLn "  1 - arbre binari de cerca (BST)"
    n <- readLn
    if n == 0
    then do putStrLn "Has triat llista (0)"
            let mem = (start :: MemList a)
            chooseTest numType prog1 prog2 mem
    else if n == 1
    then do putStrLn "Has triat BST (1)"
            let mem = (start :: MemBST a)
            chooseTest numType prog1 prog2 mem
    else do putStrLn "Has d'introduir 0 o 1"
            chooseMem numType prog1 prog2

chooseNumType :: IO ()
chooseNumType = do
    putStrLn "- Tria el tipus de valors numerics (0 o 1)"
    putStrLn "  0 - enters"
    putStrLn "  1 - reals"
    n <- readLn
    if n == 0
    then do putStrLn "Has triat enters (0)"
            prog1 <- (uploadProgram programhs1 :: IO (Command Integer))
            prog2 <- (uploadProgram programhs2 :: IO (Command Integer))
            chooseMem "enters" prog1 prog2
    else if n == 1
    then do putStrLn "Has triat reals (1)"
            prog1 <- (uploadProgram programhs1 :: IO (Command Double))
            prog2 <- (uploadProgram programhs2 :: IO (Command Double))
            chooseMem "reals" prog1 prog2
    else do putStrLn "Has d'introduir 0 o 1"
            chooseNumType


-------------------------------------
-------------------------------------
-- Main
-------------------------------------
-------------------------------------

-------------------------------------
-- P4
-------------------------------------

main = chooseNumType







-- TODO: 
-- Tubs / connectors / vectors amb mides <= 0 -> llençar errors!! <- comprovar que funciona