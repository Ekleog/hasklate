module Main where

import Control.Monad
import Data.Char
import Data.List
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import System.Environment
import Text.Printf

import Debug.Trace

data MirExp = MirInt Integer
            | MirApp MirExp MirExp
            | MirVar String
    deriving Show

data MirDecl = MirDecl {name :: String, value :: MirExp, args :: [String]}
    deriving Show

type Mir = [MirDecl]

fromHaskell :: String -> Mir
fromHaskell =
    toMir . fromHaskellParse . parseModule

fromHaskellParse :: ParseResult HsModule -> HsModule
fromHaskellParse (ParseOk p) =
    p

toMir :: HsModule -> Mir
toMir (HsModule src mod exp imp code) =
    fmap declToMir code

declToMir :: HsDecl -> MirDecl
-- declToMir (HsTypeDecl src name names type) = show
-- declToMir (HsDataDecl src context name names condecls qnames) = show
-- declToMir (HsInfixDecl src assoc int ops) = show
-- declToMir (HsNewTypeDecl src context name names condecl qnames) = show
-- declToMir (HsClassDecl src context name names decls) = show
-- declToMir (HsInstDecl src context qname types decls) = show
-- declToMir (HsDefaultDecl src types) = show
-- declToMir (HsTypeSig src names qualtype) = show
declToMir (HsFunBind [match]) =
    trace (show match) $ matchToMir match
declToMir (HsPatBind src (HsPVar pat) rhs decls) =
    trace (show (pat, rhs, decls)) $ MirDecl {name = hsName pat, value = hsValue rhs, args = []}
-- declToMir (HsForeignImport src str safety str_ name type) = show
-- declToMir (HsForeignExport src str str_ name type) = show
declToMir x =
    trace (show x) $ MirDecl {name = "UNIMPLEMENTED", value = MirInt 42, args = []} -- fallback :'(

matchToMir :: HsMatch -> MirDecl
matchToMir (HsMatch src name pats rhs decls) =
    MirDecl {name = hsName name, value = hsValue rhs, args = fmap (hsName . hsPVar) pats}

hsPVar :: HsPat -> HsName
hsPVar (HsPVar v) = v

hsName :: HsName -> String
hsName (HsIdent name) =
    name

hsValue :: HsRhs -> MirExp
hsValue (HsUnGuardedRhs v) =
    hsExp v

hsExp :: HsExp -> MirExp
hsExp (HsLit (HsInt i)) =
    MirInt i
hsExp (HsApp f x) =
    MirApp (hsExp f) (hsExp x)
hsExp (HsVar (UnQual (HsIdent x))) =
    MirVar x
hsExp (HsVar (UnQual (HsSymbol s))) =
    MirVar $ "symbol_0x" ++ (concat $ fmap (\x -> printf "%02x" (ord x)) s) ++ "_"
hsExp (HsInfixApp lhs (HsQVarOp op) rhs) =
    hsExp $ HsApp (HsApp (HsVar op) lhs) rhs

toCpp :: Mir -> String
toCpp x =
    prologueCpp ++ concat (fmap (declToCpp "") x) ++ epilogueCpp

prologueCpp :: String
prologueCpp =
   "struct unit {};\n\
   \\n\
   \template <int I>\n\
   \struct num {\n\
   \    template <typename T>\n\
   \    using value = num<I>;\n\
   \\n\
   \    static const int evaluate = I;\n\
   \};\n\
   \\n\
   \template <typename x>\n\
   \struct symbol_0x2b_ { // '+'\n\
   \    template <typename y>\n\
   \    struct result {\n\
   \        template <typename T>\n\
   \        using value = num<x::evaluate + y::evaluate>;\n\
   \    };\n\
   \\n\
   \    template <typename T>\n\
   \    using value = result<T>;\n\
   \};\n\n"

epilogueCpp :: String
epilogueCpp =
   "#include <iostream>\n\
   \int main() {\n\
   \    std::cout << main_<unit>::evaluate << '\\n';\n\
   \}\n"

-- String: prefix to put in front of all lines
declToCpp :: String -> MirDecl -> String
declToCpp p d@(MirDecl {args = []}) =
    trace (show d ++ "\n") $
    p ++ "template <typename T>\n" ++
    p ++ "using " ++ (name d) ++ " = " ++
    (expToCpp (value d) (True, True)) ++ "<T>;\n\n"
declToCpp p d@(MirDecl {args = (a:as)}) =
    trace (show d ++ "\n") $
    p ++"template <typename " ++ a ++ ">\n" ++
    p ++ "struct " ++ (name d) ++ " {\n" ++
    declToCpp (p ++ "    ") (MirDecl {name = name d ++ "I", value = value d, args = as}) ++
    p ++ "    template <typename T>\n" ++
    p ++ "    using value = " ++ name d ++ "I<T>;\n" ++
    p ++ "};\n\n"

-- (Bool, Bool): (is function immediately called, is it outmost level call)
expToCpp :: MirExp -> (Bool, Bool) -> String
expToCpp (MirInt i) _ =
    "num<" ++ show i ++ ">"
expToCpp (MirApp f x) (fc, oc) =
    (if oc then "typename " else "") ++
    (expToCpp f (True, False)) ++ "<" ++ (expToCpp x (False, False)) ++ ">::" ++
    (if fc then "template " else "") ++ "value"
expToCpp (MirVar x) _ =
    x

hasklate :: String -> String
hasklate =
    toCpp . fromHaskell

main =
    getArgs >>= mapM_ (readFile >=> (putStr . hasklate))
