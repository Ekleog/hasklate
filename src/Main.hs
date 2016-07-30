module Main where

import Control.Monad
import Data.Char
import Data.List
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import System.Environment
import Text.Printf
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Set as S

import Debug.Trace

-- Data declarations
data MirExp = MirInt Integer
            | MirApp MirExp MirExp -- f x
            | MirCons String [MirExp]
            | MirVar String
            | MirIf MirExp MirExp MirExp -- cond true false
    deriving Show

data MirArg = MirArgVar String
            | MirArgPat String [MirArg]
    deriving Show

data MirCtor = MirCtor {ctor :: String, ctorArgs :: [String]}
    deriving Show

data MirDecl = MirFunDecl  {name :: String, value :: MirExp, args :: [MirArg]}
             | MirDataDecl {name :: String, ctors :: [MirCtor]}
    deriving Show

type Mir = [MirDecl]


-- Cpp Monad
type CppMonad a = WriterT [String] (State (S.Set String)) a

tellOne :: String -> CppMonad ()
tellOne x = tell [x]

indent :: CppMonad () -> CppMonad ()
indent = censor $ fmap ("    "++)

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
    concat (fmap declToMir code)

declToMir :: HsDecl -> [MirDecl]
-- declToMir (HsTypeDecl src name names type) = show
declToMir (HsDataDecl src context name names condecls qnames) =
    [MirDataDecl {name = hsName name, ctors = fmap hsConDecl condecls}]
-- declToMir (HsInfixDecl src assoc int ops) = show
-- declToMir (HsNewTypeDecl src context name names condecl qnames) = show
-- declToMir (HsClassDecl src context name names decls) = show
-- declToMir (HsInstDecl src context qname types decls) = show
-- declToMir (HsDefaultDecl src types) = show
-- declToMir (HsTypeSig src names qualtype) = show
declToMir (HsFunBind match) =
    fmap matchToMir match
declToMir (HsPatBind src (HsPVar pat) rhs decls) =
    trace (show (pat, rhs, decls)) $ [MirFunDecl {name = hsName pat, value = hsValue rhs, args = []}]
-- declToMir (HsForeignImport src str safety str_ name type) = show
-- declToMir (HsForeignExport src str str_ name type) = show
declToMir x =
    trace (show x) $ [MirFunDecl {name = "UNIMPLEMENTED", value = MirInt 42, args = []}] -- fallback :'(

stupidArgList :: Int -> [String]
stupidArgList l = fmap ((++) "Arg" . show) [1 .. l]

matchToMir :: HsMatch -> MirDecl
matchToMir m@(HsMatch src name pats rhs decls) =
    trace (show m) $
    MirFunDecl {name = hsName name, value = hsValue rhs, args = fmap hsPat pats}

hsConDecl :: HsConDecl -> MirCtor
hsConDecl c@(HsConDecl _ name args) =
    trace (show c) $ MirCtor {ctor = hsName name, ctorArgs = stupidArgList (length args)}

hsPat :: HsPat -> MirArg
hsPat (HsPVar v) =
    MirArgVar (hsName v)
hsPat (HsPApp (UnQual name) args) =
    MirArgPat (hsName name) (fmap hsPat args)
hsPat (HsPParen x) =
    hsPat x

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
    trace (show (HsApp f x)) $ MirApp (hsExp f) (hsExp x)
hsExp (HsVar (UnQual name)) =
    MirVar (hsName name)
hsExp (HsCon (UnQual name)) =
    MirVar (hsName name)
hsExp (HsInfixApp lhs (HsQVarOp op) rhs) =
    hsExp $ HsApp (HsApp (HsVar op) lhs) rhs
hsExp (HsIf cond true false) =
    MirIf (hsExp cond) (hsExp true) (hsExp false)
hsExp (HsParen exp) =
    hsExp exp

toCpp :: Mir -> String
toCpp x =
    let cpp =
            flip evalState S.empty $
            execWriterT $
                forM x declToCpp
    in
    prologueCpp ++ unlines cpp ++ epilogueCpp


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
declToCpp :: MirDecl -> CppMonad ()
declToCpp fd@MirFunDecl{} =
    funDeclToCpp fd
declToCpp dd@MirDataDecl{} =
    dataDeclToCpp dd


declOnce :: MirDecl -> CppMonad ()
declOnce d = do
    definedNames <- get
    put $ S.insert (name d) definedNames
    when (not (null (args d)) && not (S.member (name d) definedNames)) $ do
        let as = stupidArgList (length (args d))
        tellOne $ (argListToCpp as True)
        tellOne $ "struct " ++ (name d) ++ "_impl;\n"
        tellOne $ "struct " ++ (name d) ++ " {"
        indent $ funDeclToCppInner as (name d) as
        tellOne $ "};"

funDeclToCpp :: MirDecl -> CppMonad ()
funDeclToCpp d = do
   traceM (show d ++ "\n")
   declOnce d
   tellOne $ (tplArgListToCpp (args d))
   tellOne $ "struct " ++ (name d) ++ (if not (null (args d)) then "_impl" else "") ++ (tplSpecToCpp (args d) False) ++ " {"
   tellOne $ "    using value = " ++ (expToCpp (value d) True) ++ "::value;"
   tellOne $ "};\n"

-- Send the [String] argument list twice
funDeclToCppInner :: [String] -> String -> [String] -> CppMonad ()
funDeclToCppInner [] n args = do
    tellOne $ "using value = typename " ++ n ++ "_impl" ++ (argListToCpp (fmap ((++) "typename ") $ fmap (flip (++) "::value") args) False) ++ "::value;"
funDeclToCppInner (a:as) n args = do
    tellOne $ "struct value {"
    tellOne $ "    template <typename " ++ a ++ ">"
    tellOne $ "    struct call {"
    indent $ indent $ funDeclToCppInner as n args
    tellOne $ "    };"
    tellOne $ "};\n"

-- Bool: is a definition?
argListToCpp :: [String] -> Bool -> String
argListToCpp args def =
    (\x -> (if def then "template " else "") ++ "<" ++ x ++ ">") $
    concat $ intersperse ", " $ (if def then fmap ((++) "class ") else id) args

tplArgListToCpp :: [MirArg] -> String
tplArgListToCpp args =
    if null args then "" else
    (\x -> "template <" ++ x ++ ">") $
    concat $ intersperse ", " $ fmap ((++) "class ") $ variablesToBind args

tplSpecToCpp :: [MirArg] -> Bool -> String
tplSpecToCpp args alwaysSpecializes =
    if null args || (not alwaysSpecializes && doesNotSpecialize args) then "" else
    (\x -> "<" ++ x ++ ">") $
    concat $ intersperse ", " $ fmap argToCpp args

doesNotSpecialize :: [MirArg] -> Bool
doesNotSpecialize [] = True
doesNotSpecialize ((MirArgVar _):tl) = doesNotSpecialize tl
doesNotSpecialize ((MirArgPat _ _):_) = False

variablesToBind :: [MirArg] -> [String]
variablesToBind [] = []
variablesToBind ((MirArgVar v):tl) = v : variablesToBind tl
variablesToBind ((MirArgPat p as):tl) = variablesToBind tl ++ variablesToBind as

argToCpp :: MirArg -> String
argToCpp (MirArgVar v) = v
argToCpp (MirArgPat p as) = p ++ "_impl" ++ tplSpecToCpp as True

dataDeclToCpp :: MirDecl -> CppMonad ()
dataDeclToCpp d = forM_ (ctors d) ctorToCpp

ctorArgListToCpp :: [String] -> String
ctorArgListToCpp args =
    if null args then "" else
    "<" ++ (concat $ intersperse ", " $ fmap ((++) "class ")  args) ++ ">"

ctorToCpp :: MirCtor -> CppMonad ()
ctorToCpp ct =
    if null (ctorArgs ct) then do
        tellOne $ "struct " ++ (ctor ct) ++ "_impl {};\n"
        tellOne $ "struct " ++ (ctor ct) ++ " {"
        tellOne $ "    using value = " ++ (ctor ct) ++ "_impl;"
        tellOne $ "};\n"
    else do
        let as = stupidArgList (length (ctorArgs ct))
        tellOne $ "template " ++ (ctorArgListToCpp (ctorArgs ct))
        tellOne $ "struct " ++ (ctor ct) ++ "_impl {};\n"
        tellOne $ "struct " ++ (ctor ct) ++ " {"
        indent $ ctorToCppInner as (ctor ct) as
        tellOne $ "};\n"

-- Send the [String] argument list twice
ctorToCppInner :: [String] -> String -> [String] -> CppMonad ()
ctorToCppInner [] n args =
    tellOne $ "using value = " ++ n ++ "_impl" ++ (argListToCpp args False) ++ ";"
ctorToCppInner (a:as) n args = do
    tellOne $ "struct value {"
    tellOne $ "    template <class " ++ a ++ ">"
    tellOne $ "    struct call {"
    indent $ indent $ ctorToCppInner as n args
    tellOne $ "    };"
    tellOne $ "};\n"

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
