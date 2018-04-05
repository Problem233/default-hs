module Sandbox.Format

import Sandbox.Lib
import Sandbox.HList

%access public export
%default total

data Fmt = FInt | FInteger | FNat | FDouble | FString | FLiteral Char

toFmt : String -> List Fmt
toFmt = toFmt' . unpack
  where toFmt' ('%' :: c :: r) =
          (:: toFmt' r) $ case c of
            'd' => FInt
            'l' => FInteger
            'u' => FNat
            'e' => FDouble
            's' => FString
            '%' => FLiteral '%'
            _ => assert_unreachable
        toFmt' (c :: r) = FLiteral c :: toFmt' r
        toFmt' [] = []

formatTy : List Fmt -> List Type
formatTy = flip foldr [] $ \fmt => case fmt of
             FInt => (Int ::)
             FInteger => (Integer ::)
             FNat => (Nat ::)
             FDouble => (Double ::)
             FString => (String ::)
             _ => id

formatHList : (fmtStr : String) -> HList (formatTy $ toFmt fmtStr) -> String
formatHList fmtStr = formatHList' (toFmt fmtStr)
  where formatHList' : (fmt : List Fmt) -> HList (formatTy fmt) -> String
        formatHList' (FInt :: rFmt) (x :: r) = show x ++ formatHList' rFmt r
        formatHList' (FInteger :: rFmt) (x :: r) = show x ++ formatHList' rFmt r
        formatHList' (FNat :: rFmt) (x :: r) = show x ++ formatHList' rFmt r
        formatHList' (FDouble :: rFmt) (x :: r) = show x ++ formatHList' rFmt r
        formatHList' (FString :: rFmt) (str :: r) = str ++ formatHList' rFmt r
        formatHList' (FLiteral c :: rFmt) r = c `strCons` formatHList' rFmt r
        formatHList' [] [] = ""

format : (fmtStr : String) -> curryHListTy (formatTy $ toFmt fmtStr) String
format fmtStr = curryHList $ formatHList fmtStr

printfHList : (fmtStr : String) -> HList (formatTy $ toFmt fmtStr) -> IO ()
printfHList fmtStr args = putStr $ formatHList fmtStr args

printf : (fmtStr : String) -> curryHListTy (formatTy $ toFmt fmtStr) (IO ())
printf fmtStr = curryHList $ printfHList fmtStr
