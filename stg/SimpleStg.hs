module SimpleStg (stgLowerFreeVars)
where

import StgSyn

-- prior to emitting llvm, all freevars must become explicit argumetns
stgLowerFreeVars :: [StgBinding] -> [StgBinding]
stgLowerFreeVars binds = binds --go binds (symMap)
--where
--go :: StgExpr -> a -> StgExpr
--go (StgLet binds expr) symMap =
--go (StgApp id expr) symMap =
--go e symMap = e
