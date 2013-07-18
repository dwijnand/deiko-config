module Text.Deiko.Config.Parser where

import Control.Monad
import Control.Monad.Free (Free, wrap)
import Control.Monad.Free.Church (F, fromF)
import Data.Conduit
import Data.Foldable (traverse_, foldMap)
import Text.Deiko.Config.Internal
import Text.Deiko.Config.Util

data LALR a = Shift a
            | Reduce Production a
            | LookAhead (Token -> Bool) (Bool -> a)
            | Failure (Token -> String)
            | PrintStack -- only when debugging

data Cell = CToken Position Sym
          | CAst (Mu AST)
          | CIdent Ident
          | CProp (Prop (Mu AST))
          | CProps [Prop (Mu AST)]
          | CEOF deriving Show

instance Functor LALR where
  fmap f (Shift a)       = Shift (f a)
  fmap f (Reduce p a)    = Reduce p (f a)
  fmap f (LookAhead p k) = LookAhead p (f . k)
  fmap _ (Failure e)     = Failure e
  fmap _ PrintStack      = PrintStack

type Stack = [Cell]

type Production = Stack -> (Cell, Stack)

type Transformation m = 
  (Stack, Maybe Token) -> Sink Token m (Either String ([Prop (Mu AST)]))

identSimple :: Production
identSimple ((CToken p (ID x)):xs) = (CIdent $ Ident p x, xs)

identSelect :: Production
identSelect ((CToken p (ID x)):(CToken _ DOT):(CIdent id):xs) =
  (CIdent $ Select id (Ident p x), xs)

string :: Production
string ((CToken p (STRING x)):xs) = (CAst $ Mu $ ASTRING p x, xs)
string ((CToken p (ID x)):xs)     = (CAst $ Mu $ ASTRING p x, xs) 

--identValue :: Production
--identValue ((CAst (Mu (ASTRING p x))):xs) = (Mu $ ASTRING x, xs)

list :: Production
list ((CToken _ RBRACK):(CToken p LBRACK):xs) = (CAst $ Mu $ ALIST p [], xs)
list ((CToken _ RBRACK):(CAst (Mu (ALIST p values))):xs) = 
  (CAst $ Mu $ ALIST p $ reverse values, xs)
list (x@(CToken _ RBRACK):_:xs) = list (x:xs)

listHead :: Production
listHead ((CAst value):(CToken p LBRACK):xs) = (CAst $ Mu $ ALIST p [value], xs)
listHead ((CAst value):(CToken _ COMMA):(CAst (Mu (ALIST p values))):xs) =
  (CAst $ Mu $ ALIST p (value:values), xs)
listHead (x@(CAst value):y@(CToken _ COMMA):_:xs) = listHead (x:y:xs) 
listHead (x@(CAst value):_:xs) = listHead (x:xs)

subst :: Production
subst ((CToken p (SUBST x)):xs) = (CAst $ Mu $ ASUBST p x, xs)

merge :: Production
merge ((CAst y):(CToken _ SPACE):(CAst x):xs) = (CAst $ Mu $ AMERGE x y, xs)
merge (_:xs) = merge xs

object :: Production
object ((CToken _ RBRACE):(CToken p LBRACE):xs) = (CAst $ Mu $ AOBJECT p [], xs)
object ((CToken _ RBRACE):(CProps ps):(CToken p LBRACE):xs) =
  (CAst $ Mu $ AOBJECT p $ reverse ps, xs)
object (x@(CToken _ RBRACE):y@(CProps _):_:xs) = object (x:y:xs)
object (x@(CToken _ RBRACE):_:xs) = object (x:xs)

property :: Production
property (x@(CAst _):y@(CIdent _):(CToken _ SPACE):xs) = property (x:y:xs)
property ((CAst value):(CIdent id):xs) = (CProp $ Prop id value, xs)
property (x@(CAst value):_:xs) = property (x:xs)
property (_:xs) = property xs -- trailling space, ex: id: value_[end]

propertiesHead :: Production
propertiesHead ((CProp p):xs) = (CProps [p], xs)

properties :: Production
properties ((CProp p):(CProps ps):xs)          = (CProps (p:ps), xs)
properties (x@(CProp _):(CToken _ _):xs)        = properties (x:xs)
properties ((CToken _ NEWLINE):(CProps ps):xs) = (CProps ps, xs)
properties (_:xs)                              = properties xs

shift :: F LALR ()
shift = wrap $ Shift (return ())

reduce :: Production -> F LALR ()
reduce prod = wrap $ Reduce prod (return ())

lookAhead :: (Token -> Bool) -> F LALR Bool
lookAhead p = wrap $ LookAhead p return

printStack :: F LALR a
printStack = wrap PrintStack

failure :: (Token -> String) -> F LALR a
failure k = wrap $ Failure k

isId :: Token -> Bool
isId (Elm _ _ (ID _)) = True
isId _                = False

isDot :: Token -> Bool
isDot (Elm _ _ DOT) = True
isDot _             = False

isLBrack :: Token -> Bool
isLBrack (Elm _ _ LBRACK) = True
isLBrack _                = False

isRBrack :: Token -> Bool
isRBrack (Elm _ _ RBRACK) = True
isRBrack _                = False

isLBrace :: Token -> Bool
isLBrace (Elm _ _ LBRACE) = True
isLBrace _                = False

isRBrace :: Token -> Bool
isRBrace (Elm _ _ RBRACE) = True
isRBrace _                = False

isString :: Token -> Bool
isString (Elm _ _ (STRING _)) = True
isString _                    = False

isSubst :: Token -> Bool
isSubst (Elm _ _ (SUBST _)) = True
isSubst _                   = False

isComma :: Token -> Bool
isComma (Elm _ _ COMMA) = True
isComma _               = False

isSpace :: Token -> Bool
isSpace (Elm _ _ SPACE) = True
isSpace _               = False

isNewline :: Token -> Bool
isNewline (Elm _ _ NEWLINE) = True
isNewline _                 = False

isEqual :: Token -> Bool
isEqual (Elm _ _ EQUAL) = True
isEqual _               = False

isEOF :: Token -> Bool
isEOF EOF = True
isEOF _   = False

anything :: Token -> Bool
anything _ = True

shiftSpace :: F LALR ()
shiftSpace =
  alt [(isSpace, shift)
      ,(anything, return ())]

shiftNewline :: F LALR ()
shiftNewline =
  alt [(isNewline, shift)
      ,(anything, return ())]

shiftSpaceOrNewline :: F LALR ()
shiftSpaceOrNewline = shiftSpace >> shiftNewline

parseId :: F LALR ()
parseId = do
  shift
  reduce identSimple
  alt [(isDot, parseSelect)
      ,(anything, return ())]

parseSelect :: F LALR ()
parseSelect = do
  shift
  alt [(isId, shift >> go)
      ,(anything, failure unexpected)]

    where
      go = do
        reduce identSelect
        dot <- lookAhead isDot
        when dot parseSelect

parseString :: F LALR ()
parseString = shift >> reduce string

parseProperty :: F LALR ()
parseProperty = do
  shiftSpace
  alt [(isId, parseId >> go)
      ,(anything, failure unexpected)]

  where
    go = do
      shiftSpace
      alt [(isEqual, shift >> step1)
          ,(isLBrace, parseObject)
          ,(anything, failure unexpected)]
      reduce property
    
    step1 = shiftSpace >> parseValue

parseProperties :: F LALR ()
parseProperties = do
  parseProperty
  reduce propertiesHead
  shiftSpace
  go

  where
    go =
      alt [(isComma, reduction)
          ,(isNewline, shift >> inter)
          ,(anything, return ())]

    inter = do
      shiftSpaceOrNewline
      alt [(isEOF, reduce properties)
          ,(isRBrace, reduce properties)
          ,(anything, reduction)]
      
    reduction = do
      parseProperty
      reduce properties
      go

parseObject :: F LALR ()
parseObject = do
  shift
  shiftSpaceOrNewline
  alt [(isRBrace, shift >> reduce object)
      ,(anything, parseProperties >> end)]
    
  where
    end =
      alt [(isRBrace, shift >> reduce object)
          ,(anything, failure unexpected)]

parseList :: F LALR ()
parseList = do 
  shift
  shiftSpaceOrNewline
  alt [(isRBrack, shift >> reduce list)
      ,(anything, go)]

  where
    go = do
      parseListHead
      shiftSpaceOrNewline
      alt [(isRBrack, shift >> reduce list)
          ,(anything, failure unexpected)]

parseListHead :: F LALR ()
parseListHead = do
  parseValue
  reduce listHead
  go
    where
      go = do
        shiftSpaceOrNewline
        shiftSpace -- sometimes having leading space after a newline
        alt [(isComma, step)
            ,(isRBrack, return ())
            ,(anything, failure unexpected)]

      step = shift >> shiftSpaceOrNewline >> parseValue >> reduce listHead >> go

parseMerge :: F LALR ()
parseMerge = do
  shift
  alt [(isRBrack, return ())
      ,(isRBrace, return ())
      ,(isEOF, return ())
      ,(anything, go)]

  where
    go = do
      parseValue
      reduce merge
      alt [(isSpace, parseMerge)
          ,(anything, return ())]

parseValue :: F LALR ()
parseValue = do
  alt [(isString, parseString)
      ,(isId, parseString)
      ,(isSubst, shift >> reduce subst)
      ,(isLBrack, parseList)
      ,(isLBrace, parseObject)
      ,(anything, failure unexpected)]
  alt [(isSpace, parseMerge)
      ,(anything, return ())]

alt :: [(Token -> Bool, F LALR ())] -> F LALR ()
alt []               = return ()
alt ((f, action):xs) = lookAhead f >>= go
  where
    go isF 
      | isF       = action
      | otherwise = alt xs 

recv :: Monad m => (Token -> Sink Token m a) -> Sink Token m a
recv k = await >>= \t -> maybe (error "Exhausted source") k t

toCell :: Token -> Cell
toCell (Elm l c s) = CToken (l, c) s
toCell EOF         = CEOF

makeParser :: Monad m 
           => Free LALR () 
           -> Sink Token m (Either String [Prop (Mu AST)])
makeParser instr = (cataFree pure impure instr) ([], Nothing)
  where
    pure _ (((CProps xs):_),_)  = return (Right $ reverse xs)
    
    impure (Shift k)       = shifting k
    impure (Reduce p k)    = reducing p k
    impure (LookAhead p k) = looking p k
    impure (Failure k)     = failing k
    impure PrintStack      = reporting

shifting :: Monad m => Transformation m -> Transformation m
shifting k (stack, ahead) = maybe (recv go) go ahead
  where
    go t = k (toCell t:stack, Nothing)

reducing :: Monad m => Production -> Transformation m -> Transformation m
reducing p k (stack, ahead) = 
  let (prod, stack1) = p stack in k ((prod:stack1), ahead)

looking :: Monad m 
        => (Token -> Bool)
        -> (Bool -> Transformation m)
        -> Transformation m
looking p k (stack, ahead) = maybe (recv go) go ahead
  where 
    go h = k (p h) (stack, Just h)

failing :: Monad m => (Token -> String) -> Transformation m
failing k (_, (Just h)) = return $ Left (k h) 

reporting :: Monad m => Transformation m 
reporting (stack, _) = error $ show stack

unexpected :: Token -> String
unexpected (Elm l c sym) = 
  "Unexpected token " ++ show sym ++ " at (" ++ show l ++ ", " ++ show c ++ ")" 

parser :: Monad m => Sink Token m (Either String ([Prop (Mu AST)]))
parser = makeParser (fromF parseProperties)
