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

-- Data declarations
data MirExp = MirInt Integer
            | MirApp MirExp MirExp -- f x
            | MirVar String
            | MirIf MirExp MirExp MirExp -- cond true false
    deriving Show

data MirDecl = MirDecl {name :: String, value :: MirExp, args :: [String]}
    deriving Show

type Mir = [MirDecl]

-- Symbol declarations
symbols :: [String]
symbols = ["+", "-", "*", "/", "==", "/=", "<", ">", "<=", ">="]

symbolName :: String -> String
symbolName "+" = "symbol_plus"
symbolName "-" = "symbol_minus"
symbolName "*" = "symbol_times"
symbolName "/" = "symbol_divides"
symbolName "==" = "symbol_equals"
symbolName "/=" = "symbol_different"
symbolName "<" = "symbol_lessthan"
symbolName ">" = "symbol_greaterthan"
symbolName "<=" = "symbol_lessequal"
symbolName ">=" = "symbol_greaterequal"

cppSymbol :: String -> String
cppSymbol "/=" = "!="
cppSymbol a = a

-- Actual program
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
hsName (HsSymbol symb) =
    symbolName symb

hsValue :: HsRhs -> MirExp
hsValue (HsUnGuardedRhs v) =
    hsExp v

hsExp :: HsExp -> MirExp
hsExp (HsLit (HsInt i)) =
    MirInt i
hsExp (HsApp f x) =
    MirApp (hsExp f) (hsExp x)
hsExp (HsVar (UnQual name)) =
    MirVar (hsName name)
hsExp (HsInfixApp lhs (HsQVarOp op) rhs) =
    hsExp $ HsApp (HsApp (HsVar op) lhs) rhs
hsExp (HsIf cond true false) =
    MirIf (hsExp cond) (hsExp true) (hsExp false)
hsExp (HsParen exp) =
    hsExp exp

toCpp :: Mir -> String
toCpp x =
    prologueCpp ++ concat (fmap (declToCpp "") x) ++ epilogueCpp

prologueCpp :: String
prologueCpp =
   "struct unit {};\n\
   \\n\
   \template <int I>\n\
   \struct num {\n\
   \    struct value {\n\
   \        static const int evaluate = I;\n\
   \    };\n\
   \};\n\
   \\n\
   \template <bool B>\n\
   \struct cond {\n\
   \    struct value {\n\
   \        static const bool evaluate = B;\n\
   \    };\n\
   \};\n\
   \\n\
   \\n\
   \template <bool Cond, class True, class False>\n\
   \struct if_impl;\n\
   \template <class True, class False>\n\
   \struct if_impl<true, True, False> {\n\
   \    using value = typename True::value;\n\
   \};\n\
   \template <class True, class False>\n\
   \struct if_impl<false, True, False> {\n\
   \    using value = typename False::value;\n\
   \};\n\
   \\n\
   \template <class Cond, class True, class False>\n\
   \struct if_ {\n\
   \    using value = typename if_impl<Cond::value::evaluate, True, False>::value;\n\
   \};\n\
   \\n" ++
   concat (fmap defineSymbol symbols)

defineSymbol :: String -> String
defineSymbol x =
   "struct " ++ (symbolName x) ++ " {\n\
   \    template <int a, int b>\n\
   \    struct impl {\n\
   \        static const int result = a " ++ (cppSymbol x) ++ " b;\n\
   \    };\n\
   \\n\
   \    struct value {\n\
   \        template <class x>\n\
   \        struct call {\n\
   \            struct value {\n\
   \                template <class y>\n\
   \                struct call {\n\
   \                    using value = typename num<impl<x::value::evaluate, y::value::evaluate>::result>::value;\n\
   \                };\n\
   \            };\n\
   \        };\n\
   \    };\n\
   \};\n\
   \\n"

epilogueCpp :: String
epilogueCpp =
   "#include <iostream>\n\
   \int main() {\n\
   \    std::cout << main_::value::evaluate << '\\n';\n\
   \}\n"

-- String: prefix to put in front of all lines
declToCpp :: String -> MirDecl -> String
declToCpp p d =
   trace (show d ++ "\n") $
   p ++ "struct " ++ (name d) ++ " {\n" ++
   declToCppInner (p ++ "    ") (args d) (value d) ++
   p ++ "};\n\n"

declToCppInner :: String -> [String] -> MirExp -> String
declToCppInner p [] v =
    p ++ "using value = " ++ (expToCpp v True) ++ "::value;\n"
declToCppInner p (a:as) v =
    p ++ "struct value {\n" ++
    p ++ "    template <typename " ++ a ++ ">\n" ++
    p ++ "    struct call {\n" ++
    declToCppInner (p ++ "        ") as v ++
    p ++ "    };\n" ++
    p ++ "};\n\n"

-- Bool : With maybe prefixing "typename "
expToCpp :: MirExp -> Bool -> String
expToCpp (MirInt i) _ =
    "num<" ++ show i ++ ">"
expToCpp (MirApp f x) pft =
    (if pft then "typename " else "") ++
    (expToCpp f False) ++ "::value::template call<" ++ (expToCpp x True) ++ ">"
expToCpp (MirVar x) _ =
    x
expToCpp (MirIf cond true false) pft =
    (if pft then "typename " else "") ++
    "if_<" ++ (expToCpp cond True) ++ "::value, " ++
    (expToCpp true True) ++ ", " ++ (expToCpp false True) ++ ">"

hasklate :: String -> String
hasklate =
    toCpp . fromHaskell

main =
    getArgs >>= mapM_ (readFile >=> (putStr . hasklate))
