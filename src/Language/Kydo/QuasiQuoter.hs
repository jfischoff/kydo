{-# LANGUAGE TemplateHaskell #-}
module Language.Kydo.QuasiQuoter where
import Language.Kydo.Parser
import Language.Kydo.Syntax
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

kydo :: QuasiQuoter 
kydo = QuasiQuoter exprQuoter undefined undefined undefined

exprQuoter :: String -> Q Exp
exprQuoter = lift . either (error . show) id . parseExpr

deriveLift ''Expr