#!/usr/bin/env stack
-- stack --resolver lts-14.18 script
import Data.List
import System.Environment

genVariants :: String -> String -> String -> String
genVariants name ffiname hstype =
  name ++ " :: RoundingMode -> " ++ hstype ++ "\n\
  \" ++ name ++ " rn = " ++ ffiname ++ " (fromEnum rn)\n\
  \{-# INLINE [1] " ++ name ++ " #-}\n\
  \{-# RULES\n\
  \\"" ++ name ++ "/TowardNegInf\" [~1] " ++ name ++ " TowardNegInf = " ++ ffiname ++ "_down\n\
  \\"" ++ name ++ "/TowardInf\" [~1] " ++ name ++ " TowardInf = " ++ ffiname ++ "_up\n\
  \\"" ++ name ++ "/TowardZero\" [~1] " ++ name ++ " TowardZero = " ++ ffiname ++ "_zero\n\
  \  #-}\n"

main = do
  args <- getArgs
  (name,suffix) <- case args of
                     ["Float"] -> return ("Float","_float")
                     ["Double"] -> return ("Double","_double")
                     _ -> fail "./GenFFIWrapper.hs (Float|Double)"
  let replaceFLOAT :: String -> String
      replaceFLOAT = unwords . map (\x -> if x == "FLOAT" then name else x) . words
      oneDecl :: String -> String -> String -> String
      oneDecl name ffiname ty = genVariants name (ffiname ++ suffix) (replaceFLOAT ty)
  putStr $ init $ unlines
    ["-- This file was generated by etc/GenFFIWrapper.hs"
    ,"-- DO NOT EDIT this file directly!"
    ,"{-# LANGUAGE MagicHash #-}"
    ,"{-# LANGUAGE UnliftedFFITypes #-}"
    ,"module FFIWrapper." ++ name ++ " where"
    ,"import Data.Int"
    ,"import Data.Word"
    ,"import GHC.Exts"
    ,"import Numeric.Rounded.Hardware.Base.Rounding"
    ,"import FFIImports"
    ,""
    ,oneDecl "roundedAdd" "c_rounded_add" "FLOAT -> FLOAT -> FLOAT"
    ,oneDecl "roundedSub" "c_rounded_sub" "FLOAT -> FLOAT -> FLOAT"
    ,oneDecl "roundedMul" "c_rounded_mul" "FLOAT -> FLOAT -> FLOAT"
    ,oneDecl "roundedDiv" "c_rounded_div" "FLOAT -> FLOAT -> FLOAT"
    ,oneDecl "roundedSqrt" "c_rounded_sqrt" "FLOAT -> FLOAT"
    ,oneDecl "roundedFMA" "c_rounded_fma"  "FLOAT -> FLOAT -> FLOAT -> FLOAT"
    ,oneDecl "roundedFromInt64" "c_rounded_int64_to" "Int64 -> FLOAT"
    ,oneDecl "roundedFromWord64" "c_rounded_word64_to" "Word64 -> FLOAT"
    ,oneDecl "roundedSumPtr" "c_rounded_sum_ptr" "Int -> Int -> Ptr FLOAT -> IO FLOAT"
    ,oneDecl "roundedSumByteArray" "c_rounded_sum_bytearr" "Int -> Int -> ByteArray# -> FLOAT"
    ]