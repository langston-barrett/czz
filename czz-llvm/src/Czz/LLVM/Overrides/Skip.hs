{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Czz.LLVM.Overrides.Skip
  ( overrides
  )
where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Type.Equality (testEquality, (:~:)(Refl))

import qualified Text.LLVM.AST as L

-- crucible
import           Lang.Crucible.Backend (IsSymInterface)
import qualified Lang.Crucible.Types as C

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.Intrinsics as CLLVM
import           Lang.Crucible.LLVM.MemModel (HasLLVMAnn)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.Translation (ModuleTranslation)
import qualified Lang.Crucible.LLVM.Translation as CLLVM
import           Lang.Crucible.LLVM.TypeContext (TypeContext)

overrides ::
  IsSymInterface sym =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: CLLVM.MemOptions) =>
  ModuleTranslation arch ->
  [String] ->
  [OverrideTemplate p sym arch rtp l a]
overrides trans toSkip = Maybe.mapMaybe mkOverride toSkip
  where
    llvmAst = trans Lens.^. CLLVM.modTransModule
    decls = Map.fromList (map (\d -> (L.defName d, CLLVM.declareFromDefine d)) (L.modDefines llvmAst))
    mkOverride fun = do
      decl <- Map.lookup (L.Symbol fun) decls
      CLLVM.llvmDeclToFunHandleRepr' decl $ \args ret -> do
        Refl <- testEquality ret C.UnitRepr
        return $
          CLLVM.basic_llvm_override $
            CLLVM.LLVMOverride
              { llvmOverride_declare = decl,
                llvmOverride_args = args,
                llvmOverride_ret = ret,
                llvmOverride_def = \_mvar _sym _args -> return ()
              }
