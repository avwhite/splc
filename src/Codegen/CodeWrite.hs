module Codegen.CodeWrite where

import Codegen.Codegen

instrToString :: Instr -> String
instrToString Add = "add"
instrToString Mul = "mul"
instrToString Sub = "sub"
instrToString DivI = "div"
instrToString ModI = "mod"
instrToString AndI = "and"
instrToString OrI = "or"
instrToString Eq = "eq"
instrToString Ne = "ne"
instrToString Lt = "lt"
instrToString Gt = "gt"
instrToString Le = "le"
instrToString Ge = "ge"
instrToString Halt = "halt"
instrToString (Ldc i) = "ldc " ++ show i
instrToString (Ldl i) = "ldl " ++ show i
instrToString (Label id) = id ++ ":"
instrToString (Bsr id) = "bsr " ++ id
instrToString (Bra id) = "bra " ++ id
instrToString (Link i) = "link " ++ show i
instrToString (Unlink) = "unlink"
instrToString (Ret) = "ret"
instrToString (Stl i) = "stl " ++ show i
instrToString (StrRR) = "str RR"
instrToString (LdrRR) = "ldr RR"
instrToString (Ajs i) = "ajs " ++ show i

writeInstr :: [Instr] -> IO ()
writeInstr is = mapM_ putStrLn (fmap instrToString is)
