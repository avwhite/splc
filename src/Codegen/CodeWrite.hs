module Codegen.CodeWrite where

import Codegen.Codegen

instrToString :: Instr -> String
instrToString Add = "add"
instrToString Mul = "mul"
instrToString (Ldc i) = "ldc " ++ show i
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
