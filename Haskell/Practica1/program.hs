import System.IO

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
instance (Show a) => Show (BExpr a) where
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

instance (Show a) => Show (NExpr a) where
    show (Var id)            = id
    show (Const a)           = show a
    show (Plus nExp1 nExp2)  = show nExp1 ++ " + " ++ show nExp2
    show (Minus nExp1 nExp2) = show nExp1 ++ " - " ++ show nExp2
    show (Times nExp1 nExp2) = show nExp1 ++ " * " ++ show nExp2
    show (Length id)         = "LENGTH(" ++ id ++ ")"
    show (Diameter id)       = "DIAMETER(" ++ id ++ ")"

instance (Show a) => Show (TExpr a) where
    show (TVar id)                   = id
    show (Merge tExpr1 cExpr tExpr2) = "MERGE " ++ show tExpr1 ++
                                       " " ++ show cExpr ++
                                       " " ++ show tExpr2
    show (Tube nExpr1 nExpr2)        = "TUBE " ++ show nExpr1 ++
                                       " " ++ show nExpr2

instance (Show a) => Show (CExpr a) where
    show (CVar id)         = id
    show (Connector nExpr) = "CONNECTOR " ++ show nExpr

instance (Show a) => Show (Command a) where
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

data Val a =
    NVal a
    | TVal a a
    | CVal a
    | VTVal Int [(a, a)]
    deriving (Show, Eq, Ord)

class SymTable m where
    update :: m a -> String -> Val a -> m a
    value  :: m a -> String -> Maybe (Val a)
    start  :: m a

data MemVal a =
    KeyVal Ident (Val a)
    deriving (Show)


-- Memory List

data MemList a =
    MemMap [MemVal a]
    deriving (Show)

concatMem :: MemList a -> MemList a -> MemList a
concatMem (MemMap l1) (MemMap l2) = MemMap (l1 ++ l2)

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
    start = (MemMap [])


-- Memory BST

data MemBST a =
    BSTNode (MemVal a) (MemBST a) (MemBST a)
    | BSTEmpty
    deriving (Show)

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
        | key == tKey = Just (tValue)
        | key < tKey  = value leftChild key
        | otherwise   = value rightChild key
        where KeyVal tKey tValue = tuple
    start = BSTEmpty


-------------------------------------
-------------------------------------
-- Semantics
-------------------------------------
-------------------------------------

-------------------------------------
-- P3
-------------------------------------

--interpretCommand: 
-- Interpreta un programa (Command) per una memoria (m a) i entrada ([a]) donades.
-- Retorna una tripleta amb:
--   1. La llista de totes les impresions (print i/o draw) o un missatge d'error.
--   2. La memoria resultant.
--   3. Entrada d'espres d'executar el codi.
interpretCommand :: (Num a, Ord a, SymTable m) =>
    m a -> [a] -> Command a -> ((Either String [a]), m a, [a])