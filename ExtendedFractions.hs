{-# OPTIONS -Wall -Werror #-}

module ExtendedFractions
( InfinitePrecision
, (.?)
, (/?)
, divConv
, setFixedPrecision
, setPrecision
) where 


---- ここから無限小数型関連 ----

data InfinitePrecision = Invalid | InfinitePrecision Integer [Int]
	deriving (Eq, Ord)

instance Show InfinitePrecision where
	show (InfinitePrecision x ys) = show x ++ case ys of
						[] -> ""
						_ -> "." ++ foldr (\n acc -> show n ++ acc) "" ys
	show _ = "Invalid"

-- 小数点として使用する
infixl 8 .?
(.?) :: (Integral a) => a -> a -> InfinitePrecision
x .? y
	| y < 0 = negate x .? abs y
	| otherwise = InfinitePrecision (fromIntegral x) (mkList (fromIntegral y) [])
		where mkList 0 ys = ys
		      mkList n ys = mkList (n `div` 10) ((n `mod` 10) : ys)

-- 整数同士の割り算で結果をInfinitePrecision型で返す
divConv :: (Integral a) => a -> a -> InfinitePrecision
divConv _ 0 = Invalid
divConv x y = InfinitePrecision (fromIntegral (x `div` y)) (calcFunc (fromIntegral((x `mod` y) * 10)) (fromIntegral y))
	where calcFunc 0 _ = []
	      calcFunc a b = (a `div` b) : calcFunc ((a `mod` b) * 10) b

-- InfinitePrecisionの割り算
infixl 6 /?
(/?) :: InfinitePrecision -> InfinitePrecision -> InfinitePrecision
(InfinitePrecision a bs) /? (InfinitePrecision x ys) =
		let longest = if length bs < length ys then length ys else length bs
		    carry acc n = acc * 10 + fromIntegral n
		    left = foldl carry a bs * (10 ^ (longest - length bs))
		    right = foldl carry x ys * (10 ^ (longest - length ys))
		in left `divConv` right
_ /? _ = Invalid

-- 整数の桁数
intLength :: Integer -> Int -> Int
intLength 0 0 = 1
intLength 0 n = n
intLength x n = intLength (x `div` 10) (n + 1)

-- 精度を指定して切り捨てたものを返す
setFixedPrecision :: Int -> InfinitePrecision -> InfinitePrecision
setFixedPrecision n (InfinitePrecision x ys)
	| n > 0 = InfinitePrecision x (take n ys)
	| otherwise = Invalid
setFixedPrecision _ _ = Invalid

-- 表示用。これを適用後のものをユーザーが計算に使った場合の動作は保障しない。
-- 整数桁・小数桁合わせてnで返す
-- 整数桁で既にn桁を越えていたら整数桁のみ表示
-- 整数桁と小数桁を合わせてnに達しないときは小数部に0を追加する
setPrecision :: Int -> InfinitePrecision -> InfinitePrecision
setPrecision n (InfinitePrecision x ys)
	| n <= 0 = Invalid
	| n <= intLength x 0 = InfinitePrecision x []
	| otherwise = InfinitePrecision x $ take (n - intLength x 0) (ys ++ replicate (n - intLength x 0) 0)
setPrecision _ _ = Invalid


---- 無限小数関連ここまで ----
---- 分数型関連ここから ----

